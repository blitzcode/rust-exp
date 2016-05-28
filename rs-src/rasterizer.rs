
use na::{Vector3, Vector4, Point3, Point4, Matrix3, Matrix4, Isometry3};
use na::{Norm, Diagonal, Inverse, Transpose};
use na;
use std::path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;
use std::f32;
use std::i64;
use std::f32::consts;
use stb_image::image;
use time::{PreciseTime};
use std::cmp;
use ansi_term;
use scoped_threadpool;
use std::cell::UnsafeCell;
use num_cpus;

//
// ------------------------------------------
// General Utilities & Linear Algebra Helpers
// ------------------------------------------
//

type V3F = Vector3<f32>;
type P3F = Point3<f32>;

fn deg_to_rad(deg: f32) -> f32 {
    // std::f32::to_radians() is still unstable
    deg * 0.0174532925
}

fn max3<T: PartialOrd>(a: T, b: T, c: T) -> T {
    if a > b {
        if a > c { a } else { c }
    } else {
        if b > c { b } else { c }
    }
}

fn min3<T: PartialOrd>(a: T, b: T, c: T) -> T {
    if a < b {
        if a < c { a } else { c }
    } else {
        if b < c { b } else { c }
    }
}

fn face_normal(v0: &P3F, v1: &P3F, v2: &P3F) -> V3F {
    na::normalize(&na::cross(&(*v1 - *v0), &(*v2 - *v0)))
}

fn fast_normalize(n: &V3F) -> V3F {
    // nalgbera doesn't use a reciprocal
    let l = 1.0 / (n.x * n.x + n.y * n.y + n.z * n.z).sqrt();
    Vector3::new(n.x * l, n.y * l, n.z * l)
}

fn reflect(i: &V3F, n: &V3F) -> V3F {
    // GLSL style reflection vector function
    *i - (*n * na::dot(n, i) * 2.0)
}

//
// -------------------------
// Mesh Loading & Processing
// -------------------------
//

struct Vertex {
    p:   P3F,
    n:   V3F,
    col: V3F
}

impl Vertex {
    fn new(px: f32, py: f32, pz: f32,
           nx: f32, ny: f32, nz: f32,
           r:  f32, g:  f32, b:  f32) -> Vertex {
        Vertex {
            p:   Point3 ::new(px, py, pz),
            n:   Vector3::new(nx, ny, nz),
            col: Vector3::new(r , g , b)
        }
    }
}

// Indexed triangle representation
struct Triangle {
    v0: u32,
    v1: u32,
    v2: u32
}

impl Triangle {
    fn new(v0: u32, v1: u32, v2: u32) -> Triangle {
        Triangle { v0: v0, v1: v1, v2: v2 }
    }
}

struct Mesh {
    tri:      Vec<Triangle>,
    vtx:      Vec<Vertex>,
    aabb_min: V3F,
    aabb_max: V3F
}

impl Mesh {
    fn new(tri: Vec<Triangle>, vtx: Vec<Vertex>) -> Mesh {
        // Compute AABB
        let mut mesh = Mesh { tri: tri, vtx: vtx, aabb_min: na::zero(), aabb_max: na::zero() };
        mesh.update_aabb();
        mesh
    }

    fn update_aabb(&mut self) {
        self.aabb_min = Vector3::new(f32::MAX, f32::MAX, f32::MAX);
        self.aabb_max = Vector3::new(f32::MIN, f32::MIN, f32::MIN);

        for v in &self.vtx {
            self.aabb_min.x = if self.aabb_min.x < v.p.x { self.aabb_min.x } else { v.p.x };
            self.aabb_min.y = if self.aabb_min.y < v.p.y { self.aabb_min.y } else { v.p.y };
            self.aabb_min.z = if self.aabb_min.z < v.p.z { self.aabb_min.z } else { v.p.z };
            self.aabb_max.x = if self.aabb_max.x > v.p.x { self.aabb_max.x } else { v.p.x };
            self.aabb_max.y = if self.aabb_max.y > v.p.y { self.aabb_max.y } else { v.p.y };
            self.aabb_max.z = if self.aabb_max.z > v.p.z { self.aabb_max.z } else { v.p.z };
        }
    }

    fn normalize_dimensions(&self) -> Matrix4<f32> {
        // Build a matrix to transform the mesh to a unit cube with the origin as its center

        // Translate to center
        let center = (self.aabb_min + self.aabb_max) / 2.0;
        let transf = Isometry3::new(-center, na::zero());

        // Scale to unit cube
        let extends = self.aabb_max - self.aabb_min;
        let extends_max = max3(extends.x, extends.y, extends.z);
        let extends_scale = 1.0 / extends_max;
        let scale: Matrix4<f32> =
            Diagonal::from_diagonal(&Vector4::new(extends_scale, extends_scale, extends_scale, 1.0));

        scale * na::to_homogeneous(&transf)
    }
}

// We have a few different combinations of vertex attributes in the mesh file format
#[derive(PartialEq, Debug)]
enum MeshFileType { XyzNxNyNz, XyzNxNyNzRGB, XyzRGB }

fn load_mesh(file_name: &str, mesh_file_type: MeshFileType) -> Mesh {
    // Load a text format mesh from disk

    let file_name = &file_name.to_string();

    let path    = path::Path::new(file_name);
    let display = path.display();

    // Open mesh file
    let mut file = match File::open(&path) {
        Err(why) => panic!("load_mesh(): Couldn't open {}: {}",
                           display,
                           Error::description(&why)),
        Ok(file) => file
    };

    // Read entire mesh file into memory
    let mut data_str = String::new();
    match file.read_to_string(&mut data_str) {
        Err(why) => panic!("load_mesh(): Couldn't read {}: {}",
                           display,
                           Error::description(&why)),
        Ok(_)    => ()
    };

    // Parse mesh format line-by-line
    let mut lines = data_str.lines();

    // Comment header / vertex count
    let vtx_cnt;
    loop {
        match lines.next() {
            Some("") => (), // Skip empty lines
            Some(ln) => {
                let words: Vec<&str> = ln.split(" ").collect();
                if words[0] == "#" { continue } // Skip comments
                // First non-empty, non-comment line should contain the vertex count
                vtx_cnt = match words[0].parse::<u32>() {
                    Err(why) => panic!("load_mesh(): Can't parse vertex count: {}: {}",
                                       Error::description(&why),
                                       display),
                    Ok(n)    => n
                };
                break;
            }
            None => panic!("load_mesh(): EOF while parsing vertex count: {}", display)
        }
    }

    // Vertices
    let mut vtx: Vec<Vertex> = Vec::new();
    loop {
        match lines.next() {
            Some("") => (), // Skip empty lines
            Some(ln) => {
                let mut words = ln.split(" ");
                let words_collect: Vec<&str> = words.clone().collect();
                let num_components = match mesh_file_type {
                    MeshFileType::XyzNxNyNzRGB => 9,
                    MeshFileType::XyzNxNyNz | MeshFileType::XyzRGB => 6
                };
                if words_collect.len() != num_components {
                    panic!("load_mesh(): Expected {} component vertices: {}",
                        num_components, display);
                }
                let mut components: Vec<f32> = Vec::new();
                for _ in 0..num_components {
                    components.push(words.next().unwrap().parse::<f32>().unwrap());
                }
                match mesh_file_type {
                    MeshFileType::XyzRGB =>
                        vtx.push(Vertex::new(components[0],
                                             components[1],
                                             components[2],
                                             // Compute the normal later
                                             0.0, 0.0, 0.0,
                                             components[3],
                                             components[4],
                                             components[5]
                                            )),
                    MeshFileType::XyzNxNyNzRGB =>
                        vtx.push(Vertex::new(components[0],
                                             components[1],
                                             components[2],
                                             components[3],
                                             components[4],
                                             components[5],
                                             components[6],
                                             components[7],
                                             components[8]
                                            )),
                    MeshFileType::XyzNxNyNz =>
                        vtx.push(Vertex::new(components[0],
                                             components[1],
                                             components[2],
                                             components[3],
                                             components[4],
                                             components[5],
                                             // White as default color
                                             1.0, 1.0, 1.0
                                            ))
                }
                // Done?
                if vtx.len() == vtx_cnt as usize { break }
            }
            None => panic!("load_mesh(): EOF while parsing vertices: {}", display)
        }
    }
    if vtx_cnt < 3 {
        panic!("load_mesh(): Bogus vertex count: {}: {}", vtx_cnt, display)
    }

    // Index count
    let idx_cnt;
    loop {
        match lines.next() {
            Some("") => (), // Skip empty lines
            Some(ln) => {
                // First non-empty, non-comment line should contain the index count
                idx_cnt = match ln.parse::<u32>() {
                    Err(why) => panic!("load_mesh(): Can't parse index count: {}: {}",
                                       Error::description(&why),
                                       display),
                    Ok(n)    => n
                };
                break;
            }
            None => panic!("load_mesh(): EOF while parsing index count: {}", display)
        }
    }
    if idx_cnt % 3 != 0 {
        panic!("load_mesh(): Bogus index count: {}: {}", idx_cnt, display)
    }

    // Indices
    let mut idx: Vec<(u32, u32, u32)> = Vec::new();
    loop {
        match lines.next() {
            Some("") => (), // Skip empty lines
            Some(ln) => {
                let mut words = ln.split(" ");
                let words_collect: Vec<&str> = words.clone().collect();
                if words_collect.len() != 3 {
                    panic!("load_mesh(): Expected 3 component indexed triangles: {}", display);
                }
                let mut components: Vec<u32> = Vec::new();
                for _ in 0..3 {
                    let idx = words.next().unwrap().parse::<u32>().unwrap();
                    if idx >= vtx_cnt {
                        panic!("load_mesh(): Out-of-bounds index: {}: {}", idx, display)
                    }
                    components.push(idx);
                }
                idx.push((components[0], components[1], components[2]));
                // Done?
                if idx.len() * 3 == idx_cnt as usize { break }
            }
            None => panic!("load_mesh(): EOF while parsing indices: {}", display)
        }
    }

    // Assemble triangle vector and mesh
    let mut tri = Vec::new();
    for tri_idx in idx {
        let ntri = Triangle::new(tri_idx.0, tri_idx.1, tri_idx.2);
        if mesh_file_type == MeshFileType::XyzRGB {
            // Set vertex normals from face normal
            // TODO: This obviously does not take sharing into account at all, but it's
            //       OK for now as we're only using it with very simple meshes
            let v0p = Point3::new(vtx[tri_idx.0 as usize].p.x,
                                  vtx[tri_idx.0 as usize].p.y,
                                  vtx[tri_idx.0 as usize].p.z);
            let v1p = Point3::new(vtx[tri_idx.1 as usize].p.x,
                                  vtx[tri_idx.1 as usize].p.y,
                                  vtx[tri_idx.1 as usize].p.z);
            let v2p = Point3::new(vtx[tri_idx.2 as usize].p.x,
                                  vtx[tri_idx.2 as usize].p.y,
                                  vtx[tri_idx.2 as usize].p.z);
            let n = face_normal(&v0p, &v1p, &v2p);
            vtx[tri_idx.0 as usize].n = n;
            vtx[tri_idx.1 as usize].n = n;
            vtx[tri_idx.2 as usize].n = n;
        }
        tri.push(ntri);
    }
    let mesh = Mesh::new(tri, vtx);

    // Print some mesh information
    println!("load_mesh(): Loaded {} Tri and {} Vtx (format: {:?}) from '{}', \
             AABB ({}, {}, {}) - ({}, {}, {})",
        mesh.tri.len(), mesh.vtx.len(), mesh_file_type, display,
        mesh.aabb_min.x, mesh.aabb_min.y, mesh.aabb_min.z,
        mesh.aabb_max.x, mesh.aabb_max.y, mesh.aabb_max.z);

    mesh
}

#[no_mangle]
pub extern fn rast_get_num_meshes() -> i32 { 12 }

#[no_mangle]
pub extern fn rast_get_mesh_name(idx: i32) -> *const u8 { mesh_by_idx(idx).0.as_ptr() }

#[no_mangle]
pub extern fn rast_get_mesh_tri_cnt(idx: i32) -> i32 { mesh_by_idx(idx).2.tri.len() as i32 }

fn mesh_by_idx<'a>(idx: i32) -> (&'a str, CameraFromTime, &'a Mesh) {
    // Retrieve mesh name, camera and geometry by its index. We do this in such an awkward
    // way so we can take advantage of the on-demand loading of the meshes through
    // lazy_static

    // Mesh geometry
    lazy_static! {
        static ref MESH_KILLEROO: Mesh =
            load_mesh("meshes/killeroo_ao.dat"      , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_HEAD: Mesh =
            load_mesh("meshes/head_ao.dat"          , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_MITSUBA: Mesh =
            load_mesh("meshes/mitsuba_ao.dat"       , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_CAT: Mesh =
            load_mesh("meshes/cat_ao.dat"           , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_HAND: Mesh =
            load_mesh("meshes/hand_ao.dat"          , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_TEAPOT: Mesh =
            load_mesh("meshes/teapot.dat"           , MeshFileType::XyzNxNyNz   );
        static ref MESH_TORUS_KNOT: Mesh =
            load_mesh("meshes/torus_knot.dat"       , MeshFileType::XyzNxNyNz   );
        static ref MESH_DWARF: Mesh =
            load_mesh("meshes/dwarf.dat"            , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_BLOB: Mesh =
            load_mesh("meshes/blob.dat"             , MeshFileType::XyzNxNyNz   );
        static ref MESH_CUBE: Mesh =
            load_mesh("meshes/cube.dat"             , MeshFileType::XyzNxNyNzRGB);
        static ref MESH_SPHERE: Mesh =
            load_mesh("meshes/sphere.dat"           , MeshFileType::XyzNxNyNz   );
        static ref MESH_CORNELL: Mesh =
            load_mesh("meshes/cornell_radiosity.dat", MeshFileType::XyzRGB      );
    }

    // Name, camera and geometry tuple
    match idx {
        // Null terminated names so we can easily pass them as C strings
        0  => ("Killeroo\0"  , cam_orbit_front,  &MESH_KILLEROO  ),
        1  => ("Head\0"      , cam_orbit_closer, &MESH_HEAD      ),
        2  => ("Mitsuba\0"   , cam_pan_front,    &MESH_MITSUBA   ),
        3  => ("Cat\0"       , cam_orbit_closer, &MESH_CAT       ),
        4  => ("Hand\0"      , cam_orbit_closer, &MESH_HAND      ),
        5  => ("Teapot\0"    , cam_orbit_closer, &MESH_TEAPOT    ),
        6  => ("TorusKnot\0" , cam_orbit,        &MESH_TORUS_KNOT),
        7  => ("Dwarf\0"     , cam_orbit_front,  &MESH_DWARF     ),
        8  => ("Blob\0"      , cam_orbit,        &MESH_BLOB      ),
        9  => ("Cube\0"      , cam_orbit,        &MESH_CUBE      ),
        10 => ("Sphere\0"    , cam_orbit,        &MESH_SPHERE    ),
        11 => ("CornellBox\0", cam_pan_back ,    &MESH_CORNELL   ),
        _  => panic!("mesh_by_idx: Invalid index: {}", idx)
    }
}

//
// -----------------
// Camera Animations
// -----------------
//

// Eye position at the given time
type CameraFromTime = fn(f64) -> P3F;

fn cam_orbit(tick: f64) -> P3F {
    // Orbit around object
    Point3::new(((tick / 1.25).cos() * 1.8) as f32,
                0.0,
                ((tick / 1.25).sin() * 1.8) as f32)
}

fn cam_orbit_closer(tick: f64) -> P3F {
    // Orbit closer around object
    Point3::new(((tick / 1.25).cos() * 1.6) as f32,
                0.0,
                ((tick / 1.25).sin() * 1.6) as f32)
}

fn cam_orbit_front(tick: f64) -> P3F {
    // Slow, dampened orbit around the front of the object, some slow vertical bobbing as well
    let tick_slow = tick / 3.5;
    let reverse   = tick_slow as i64 % 2 == 1;
    let tick_f    = if reverse {
                        1.0 - tick_slow.fract()
                    } else {
                        tick_slow.fract()
                    } as f32;
    let smooth    = smootherstep(0.0, 1.0, tick_f);
    let a_weight  = 1.0 - smooth;
    let b_weight  = smooth;
    let tick_seg  = -consts::PI / 2.0 -
                    (-(consts::PI / 6.0) * a_weight + (consts::PI / 6.0) * b_weight);
    Point3::new(tick_seg.cos() as f32,
                ((tick / 2.0).sin() * 0.25 + 0.2) as f32,
                tick_seg.sin() as f32)
}

fn cam_pan_front(tick: f64) -> P3F {
    // Camera makes circular motion looking at the mesh
    Point3::new((tick.cos() * 0.3) as f32,
                (tick.sin() * 0.3) as f32 + 0.4,
                1.7)
}

fn cam_pan_back(tick: f64) -> P3F {
    // Camera makes circular motion looking at the box (which is open at the back)
    Point3::new((tick.cos() * 0.3) as f32,
                (tick.sin() * 0.3) as f32,
                -2.0)
}

fn smootherstep(edge0: f32, edge1: f32, x: f32) -> f32
{
    // Scale and clamp x to 0..1 range
    let x = na::clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    // Evaluate polynomial
    x * x * x * (x * (x * 6.0 - 15.0) + 10.0)
}

//
// -----------------------------
// Cube Map Loading & Processing
// -----------------------------
//

// All our irradiance cube map faces have the same fixed dimensions
static CM_FACE_WDH: i32 = 64;

#[derive(PartialEq, Debug, Copy, Clone)]
enum CMFaceName { XPos, XNeg, YPos, YNeg, ZPos, ZNeg }

// TODO: We could store the different convolution cube maps interleaved,
//       increasing cache usage for lookups using multiple powers
type CMFace = Vec<V3F>;
type CM     = [CMFace; 6];

struct IrradianceCMSet {
    cos_0:     CM,       // Reflection map
    cos_1:     CM,       // Diffuse
    cos_8:     CM,       // Specular pow^x
    cos_64:    CM,       // ..
    cos_512:   CM,       // ..
    cross:     Vec<u32>, // Image of unfolded LDR cube map cross
    cross_wdh: i32,      // Width of cross
    cross_hgt: i32       // Height of cross
}

impl IrradianceCMSet {
    fn from_path(path: &str) -> IrradianceCMSet  {
        // Build a full irradiance cube map set with preview from the files found in 'path'

        let path = &path.to_string();

        println!("IrradianceCMSet::from_path(): Loading 5x6x{}x{} cube map faces of \
                 cos^[0|1|8|64|512] convolved irradiance from '{}'",
                 CM_FACE_WDH, CM_FACE_WDH, path);

        // Low-res reflection map and LDR unfolded image
        let cos_0 = load_cm(0, path);
        let (cross, cross_wdh, cross_hgt) = draw_cm_cross_buffer(&cos_0);

        IrradianceCMSet {
            cos_0:     cos_0,
            cos_1:     load_cm(1,   path),
            cos_8:     load_cm(8,   path),
            cos_64:    load_cm(64,  path),
            cos_512:   load_cm(512, path),
            cross:     cross,
            cross_wdh: cross_wdh,
            cross_hgt: cross_hgt
        }
    }

    fn draw_cross(&self, xorg: i32, yorg: i32, w: i32, h: i32, fb: *mut u32) {
        // Draw the cross image into the given framebuffer

        let x1 = na::clamp(xorg, 0, w);
        let y1 = na::clamp(yorg, 0, h);
        let x2 = cmp::min(x1 + self.cross_wdh, w);
        let y2 = cmp::min(y1 + self.cross_hgt, h);

        let cross_ptr = self.cross.as_ptr();

        for y in y1..y2 {
            let cy        = y - y1;
            let fb_row    = y * w;
            let cross_row = cy * self.cross_wdh - x1;
            for x in x1..x2 {
                let c  = unsafe { * cross_ptr.offset((cross_row + x) as isize) };

                // Skip pixels not on the cross (alpha == 0)
                if c & 0xFF000000 == 0 { continue }

                unsafe { * fb.offset((fb_row + x) as isize) = c }
            }
        }
    }
}

fn load_hdr(file_name: &String) -> image::Image<f32> {
    // Load a Radiance HDR image using the stb_image library

    let path = path::Path::new(file_name);
    if !path.exists() {
        panic!("load_hdr(): File not found: {}", file_name)
    }

    match image::load(path) {
        image::LoadResult::ImageF32(img) => img,
        image::LoadResult::ImageU8(_)    => panic!("load_hdr(): Not HDR: {}", file_name),
        image::LoadResult::Error(err)    => panic!("load_hdr(): {}: {}", err, file_name)
    }
}

fn cm_fn_from_param(path: &String, power: i32, face: CMFaceName) -> String {
    // Construct a file name like 'data/env_cos_64_x+.hdr' from the given parameters

    let face_name = match face  {
        CMFaceName::XPos => "x+",
        CMFaceName::XNeg => "x-",
        CMFaceName::YPos => "y+",
        CMFaceName::YNeg => "y-",
        CMFaceName::ZPos => "z+",
        CMFaceName::ZNeg => "z-"
    }.to_string();

    format!("{}/env_cos_{}_{}.hdr", path, power, face_name)
}

fn load_cm_face(file_name: &String, flip_x: bool, flip_y: bool) -> CMFace {
    // Load HDR
    let img = load_hdr(&file_name);
    if img.width  != CM_FACE_WDH as usize ||
       img.height != CM_FACE_WDH as usize {
        panic!("load_cm_face(): HDR image has wrong cube map face dimensions: {}: {} x {}",
              file_name, img.width, img.height);
       }

    // Convert to our format, flip axis as requested
    let mut face = Vec::new();
    face.resize((CM_FACE_WDH * CM_FACE_WDH) as usize, na::zero());
    for y in 0..CM_FACE_WDH {
        for x in 0..CM_FACE_WDH {
            face[(if flip_x { CM_FACE_WDH - 1 - x } else { x } +
                  if flip_y { CM_FACE_WDH - 1 - y } else { y } * CM_FACE_WDH) as usize] =
                Vector3::new(img.data[(x * 3 + y * CM_FACE_WDH * 3 + 0) as usize],
                          img.data[(x * 3 + y * CM_FACE_WDH * 3 + 1) as usize],
                          img.data[(x * 3 + y * CM_FACE_WDH * 3 + 2) as usize]);
        }
    }

    face
}

fn load_cm(power: i32, path: &String) -> CM {
    // Load all six cube map faces of the given power from the given path

    // The cube maps we load are oriented like OpenGL expects them, which is actually
    // rather strange. Flip and mirror so it's convenient for the way we do lookups
    [ load_cm_face(&cm_fn_from_param(path, power, CMFaceName::XPos), true , true ),
      load_cm_face(&cm_fn_from_param(path, power, CMFaceName::XNeg), false, true ),
      load_cm_face(&cm_fn_from_param(path, power, CMFaceName::YPos), false, false),
      load_cm_face(&cm_fn_from_param(path, power, CMFaceName::YNeg), false, true ),
      load_cm_face(&cm_fn_from_param(path, power, CMFaceName::ZPos), false, true ),
      load_cm_face(&cm_fn_from_param(path, power, CMFaceName::ZNeg), true , true )
    ]
}

fn draw_cm_cross_buffer(cm: &CM) -> (Vec<u32>, i32, i32) {
    // Draw a flattened out cube map into a buffer, faces are half size
    //
    //        _         _  (cross_wdh, cross_hgt)
    //       |   Y+      |
    //        X- Z- X+ Z+
    //       |_  Y-     _|
    // (0,0)
    //

    let faces = [
        CMFaceName::XPos, CMFaceName::XNeg,
        CMFaceName::YPos, CMFaceName::YNeg,
        CMFaceName::ZPos, CMFaceName::ZNeg
    ];

    let     cross_wdh = 4 * (CM_FACE_WDH / 2);
    let     cross_hgt = 3 * (CM_FACE_WDH / 2);
    let mut cross     = Vec::new();
    cross.resize((cross_wdh * cross_hgt) as usize, 0);

    for face_idx in faces.iter() {
        let face = &cm[*face_idx as usize];

        // Our faces are oriented so we can most efficiently do vector lookups, not
        // necessarily in the right format for display, so we have to mirror and flip
        let (xoff, yoff, flip_x, flip_y) = match face_idx {
            &CMFaceName::XPos => (2, 1, false, false),
            &CMFaceName::XNeg => (0, 1, true , false),
            &CMFaceName::YPos => (1, 2, false, false),
            &CMFaceName::YNeg => (1, 0, false, true ),
            &CMFaceName::ZPos => (3, 1, true , false),
            &CMFaceName::ZNeg => (1, 1, false, false)
        };

        let wdh_half = CM_FACE_WDH / 2;

        for yf in 0..wdh_half {
            for xf in 0..wdh_half {
                let x = xf + xoff * wdh_half;
                let y = yf + yoff * wdh_half;
                let col = face[(
                    if flip_x { wdh_half - 1 - xf } else { xf } * 2 +
                    if flip_y { wdh_half - 1 - yf } else { yf } * 2 * CM_FACE_WDH) as usize];

                let idx = (x + y * cross_wdh) as usize;

                // We later use the alpha channel to skip pixels outside the cross
                cross[idx] = rgbf_to_abgr32_gamma(col.x, col.y, col.z) | 0xFF000000;
            }
        }
    }

    (cross, cross_wdh, cross_hgt)
}

fn cm_texel_from_dir(dir: &V3F) -> (CMFaceName, i32)  {
    // Find the closest cube map texel pointed at by 'dir'

    let face;
    let mut u;
    let mut v;
    let dir_abs = Vector3::new(dir.x.abs(), dir.y.abs(), dir.z.abs());

    // Find major axis
    if dir_abs.x > dir_abs.y && dir_abs.x > dir_abs.z {
        face = if dir.x > 0.0 { CMFaceName::XPos } else { CMFaceName::XNeg };
        let inv_dir_abs = 1.0 / dir_abs.x;
        u    = dir.z * inv_dir_abs;
        v    = dir.y * inv_dir_abs;
    } else if dir_abs.y > dir_abs.x && dir_abs.y > dir_abs.z {
        face = if dir.y > 0.0 { CMFaceName::YPos } else { CMFaceName::YNeg };
        let inv_dir_abs = 1.0 / dir_abs.y;
        u    = dir.x * inv_dir_abs;
        v    = dir.z * inv_dir_abs;
    } else {
        face = if dir.z > 0.0 { CMFaceName::ZPos } else { CMFaceName::ZNeg };
        let inv_dir_abs = 1.0 / dir_abs.z;
        u    = dir.x * inv_dir_abs;
        v    = dir.y * inv_dir_abs;
    }

    // Face texel coordinates
    u      = (u + 1.0) * 0.5;
    v      = (v + 1.0) * 0.5;
    let tx = na::clamp((u * CM_FACE_WDH as f32) as i32, 0, CM_FACE_WDH - 1);
    let ty = na::clamp((v * CM_FACE_WDH as f32) as i32, 0, CM_FACE_WDH - 1);

    (face, tx + ty * CM_FACE_WDH)
}

fn lookup_texel_cm(cm: &CM, texel: (CMFaceName, i32)) -> V3F  {
    let (face, idx) = texel;
    unsafe { *cm.get_unchecked(face as usize).get_unchecked(idx as usize) }
}

fn lookup_dir_cm(cm: &CM, dir: &V3F) -> V3F {
    lookup_texel_cm(cm, cm_texel_from_dir(dir))
}

#[allow(dead_code)]
fn cm_texel_to_dir(face: CMFaceName, x: i32, y: i32) -> V3F {
    // Convert a texel position on a cube map face into a direction

    let vw = (x as f32 + 0.5) / CM_FACE_WDH as f32 * 2.0 - 1.0;
    let vh = (y as f32 + 0.5) / CM_FACE_WDH as f32 * 2.0 - 1.0;

    na::normalize(&match face {
        CMFaceName::XPos => V3F::new( 1.0,   vh,   vw),
        CMFaceName::XNeg => V3F::new(-1.0,   vh,   vw),
        CMFaceName::YPos => V3F::new(  vw,  1.0,   vh),
        CMFaceName::YNeg => V3F::new(  vw, -1.0,   vh),
        CMFaceName::ZPos => V3F::new(  vw,   vh,  1.0),
        CMFaceName::ZNeg => V3F::new(  vw,   vh, -1.0)
    })
}

// Normalization cube map, used as a lookup table for vector normalization
lazy_static! {
    static ref _CM_NORMALIZE:[Vec<V3F>; 6] = {
        let build_face = |face: CMFaceName| -> Vec<V3F> {
            let mut v: Vec<V3F> = Vec::new();
            v.resize((CM_FACE_WDH * CM_FACE_WDH) as usize, V3F::new(0.0, 0.0, 0.0));
            for y in 0..CM_FACE_WDH {
                for x in 0..CM_FACE_WDH {
                    v[(x + y * CM_FACE_WDH) as usize] = cm_texel_to_dir(face, x, y);
                }
            }
            v
        };

        [ build_face(CMFaceName::XPos), build_face(CMFaceName::XNeg),
          build_face(CMFaceName::YPos), build_face(CMFaceName::YNeg),
          build_face(CMFaceName::ZPos), build_face(CMFaceName::ZNeg)
        ]
    };
}

#[no_mangle]
pub extern fn rast_get_num_cm_sets() -> i32 { 9 }

#[no_mangle]
pub extern fn rast_get_cm_set_name(idx: i32) -> *const u8 { cm_set_by_idx(idx).0.as_ptr() }

fn cm_set_by_idx<'a>(idx: i32) -> (&'a str, &'a IrradianceCMSet) {
    // Retrieve irradiance cube map set name and images by its index. We do this in such
    // an awkward way so we can take advantage of the on-demand loading of the cube maps
    // through lazy_static

    // Sets of pre-filtered irradiance cube maps
    lazy_static! {
        static ref CM_GRACE: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/grace"      );
        static ref CM_PARKING_LOT: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/parking_lot");
        static ref CM_ENIS: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/enis"       );
        static ref CM_GLACIER: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/glacier"    );
        static ref CM_PISA: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/pisa"       );
        static ref CM_PINE_TREE: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/pine_tree"  );
        static ref CM_UFFIZI: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/uffizi"     );
        static ref CM_DOGE: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/doge"       );
        static ref CM_COLTEST: IrradianceCMSet =
            IrradianceCMSet::from_path("envmaps/coltest/"   );
    }

    match idx {
        // Null terminated names so we can easily pass them as C strings
        0 => ("Grace\0"     , &CM_GRACE      ),
        1 => ("ParkingLot\0", &CM_PARKING_LOT),
        2 => ("Enis\0"      , &CM_ENIS       ),
        3 => ("Glacier\0"   , &CM_GLACIER    ),
        4 => ("Pisa\0"      , &CM_PISA       ),
        5 => ("PineTree\0"  , &CM_PINE_TREE  ),
        6 => ("Uffizi\0"    , &CM_UFFIZI     ),
        7 => ("Doge\0"      , &CM_DOGE       ),
        8 => ("ColTest\0"   , &CM_COLTEST    ),
        _ => panic!("cm_set_by_idx: Invalid index: {}", idx)
    }
}

//
// -------
// Shaders
// -------
//

// All shaders have this signature
type Shader = fn(&V3F,                // World space position
                 &V3F,                // World space normal
                 &V3F,                // Color (usually baked AO / radiosity)
                 &P3F,                // World space camera position
                 f64,                 // Current time (tick)
                 &IrradianceCMSet) -> // Current environment cube map set
                 V3F;                 // Output color

fn shader_color(_: &V3F, _: &V3F, col: &V3F, _: &P3F, _: f64, _: &IrradianceCMSet) -> V3F {
    *col
}

fn shader_n_to_color(_: &V3F, n: &V3F, _: &V3F, _: &P3F, _: f64, _: &IrradianceCMSet) -> V3F {
    // Convert the normal to a color
    (n.normalize() + 1.0) * 0.5
}

fn shader_headlight(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _: f64, _: &IrradianceCMSet) -> V3F {
    let n         = fast_normalize(n);
    let l         = fast_normalize(&(*eye.as_vector() - *p));
    let ldotn     = na::clamp(na::dot(&l, &n), 0.0, 1.0);
    let occlusion = *col * *col;
    occlusion * ldotn
}

fn shader_dir_light(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                    _cm: &IrradianceCMSet) -> V3F {
    // Specular material lit by two light sources

    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vector();
    let r   = fast_normalize(&reflect(&eye, &n));
    let l   = Vector3::new(0.577350269, 0.577350269, 0.577350269); // Normalized (1, 1, 1)

    let light_1 = {
        let ldotn =                 na::clamp(na::dot(&l, &n), 0.0, 1.0);
        let ldotr = fast_unit_pow16(na::clamp(na::dot(&l, &r), 0.0, 1.0));
        ldotn * 0.25 + ldotr * 0.75
    };

    let light_2 = {
        let ldotn =                 na::clamp(na::dot(&-l, &n), 0.0, 1.0);
        let ldotr = fast_unit_pow16(na::clamp(na::dot(&-l, &r), 0.0, 1.0));
        ldotn * 0.25 + ldotr * 0.75
    };

    let ambient   = Vector3::new(0.05, 0.05, 0.05);
    let light     = Vector3::new(1.0, 0.5, 0.5) * light_1 +
                    Vector3::new(0.5, 0.5, 1.0) * light_2 +
                    ambient;
    let occlusion = *col * *col;

    light * occlusion
}

fn normalize_phong_lobe(power: f32) -> f32
{
    (power + 2.0) * 0.5
}

fn shader_cm_diffuse(_p: &V3F, n: &V3F, col: &V3F, _eye: &P3F, _tick: f64,
                     cm: &IrradianceCMSet) -> V3F {
    let n = fast_normalize(n);
    lookup_dir_cm(&cm.cos_1, &n) * (*col * *col)
}

fn shader_cm_refl(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                  cm: &IrradianceCMSet) -> V3F {
    let n     = fast_normalize(n);
    let eye   = *p - *eye.as_vector();
    let r     = reflect(&eye, &n);
    let r_tex = cm_texel_from_dir(&r);

    ( lookup_dir_cm  (&cm.cos_1 , &n)
    + lookup_texel_cm(&cm.cos_8 , r_tex) * normalize_phong_lobe(8.0 )
    + lookup_texel_cm(&cm.cos_64, r_tex) * normalize_phong_lobe(64.0)
    )
    * (*col * *col)
}

fn shader_cm_coated(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                    cm: &IrradianceCMSet) -> V3F {
    let n       = fast_normalize(n);
    let eye     = *p - *eye.as_vector();
    let r       = reflect(&eye, &n);
    let r_tex   = cm_texel_from_dir(&r);
    let fresnel = fresnel_conductor(na::dot(&-eye, &n), 1.0, 1.1);

    ( lookup_dir_cm  (&cm.cos_1  , &n)                                            * 0.85
    + lookup_texel_cm(&cm.cos_8  , r_tex) * normalize_phong_lobe(8.0  ) * fresnel
    + lookup_texel_cm(&cm.cos_512, r_tex) * normalize_phong_lobe(512.0) * fresnel * 1.5
    )
    * (*col * *col)
}

fn shader_cm_diff_rim(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _: f64,
                      cm: &IrradianceCMSet) -> V3F {
    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vector();

    let fresnel = fresnel_conductor(na::dot(&-eye, &n), 1.0, 1.1);

    (lookup_dir_cm(&cm.cos_1, &n) + fresnel * 0.75) * *col
}

fn shader_cm_glossy(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                    cm: &IrradianceCMSet) -> V3F {
    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vector();
    let r   = reflect(&eye, &n);

    ( lookup_dir_cm(&cm.cos_1, &n)
    + lookup_dir_cm(&cm.cos_8, &r) * normalize_phong_lobe(8.0)
    )
    * (*col * *col)
}

fn shader_cm_green_highlight(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                             cm: &IrradianceCMSet) -> V3F {
    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vector();
    let r   = reflect(&eye, &n);

    ( lookup_dir_cm(&cm.cos_1 , &n)
    + lookup_dir_cm(&cm.cos_64, &r) * normalize_phong_lobe(64.0) * Vector3::new(0.2, 0.8, 0.2)
    )
    * (*col * *col)
}

fn shader_cm_red_material(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                          cm: &IrradianceCMSet) -> V3F {
    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vector();
    let r   = reflect(&eye, &n);

    ( lookup_dir_cm(&cm.cos_1  , &n) * Vector3::new(0.8, 0.2, 0.2)
    + lookup_dir_cm(&cm.cos_512, &r) * normalize_phong_lobe(512.0)
    )
    * (*col * *col)
}

fn shader_cm_metallic(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                      cm: &IrradianceCMSet) -> V3F {
    let n     = fast_normalize(n);
    let eye   = *p - *eye.as_vector();
    let r     = reflect(&eye, &n);
    let r_tex = cm_texel_from_dir(&r);

    ( lookup_texel_cm(&cm.cos_8 , r_tex) * normalize_phong_lobe(8.0 )
    + lookup_texel_cm(&cm.cos_64, r_tex) * normalize_phong_lobe(64.0)
    )
    * (*col)
}

fn shader_cm_super_shiny(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                         cm: &IrradianceCMSet) -> V3F {
    let n     = fast_normalize(n);
    let eye   = *p - *eye.as_vector();
    let r     = reflect(&eye, &n);
    let r_tex = cm_texel_from_dir(&r);

    ( lookup_texel_cm(&cm.cos_64 , r_tex) * normalize_phong_lobe(64.0 )
    + lookup_texel_cm(&cm.cos_512, r_tex) * normalize_phong_lobe(512.0)
    + lookup_texel_cm(&cm.cos_0  , r_tex)
    )
    * (*col)
}

fn shader_cm_gold(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                  cm: &IrradianceCMSet) -> V3F {
    let n      = fast_normalize(n);
    let l      = fast_normalize(&(*eye.as_vector() - *p));
    let ldotn  = na::clamp(na::dot(&l, &n), 0.0, 1.0);
    let eye    = *p - *eye.as_vector();
    let r      = reflect(&eye, &n);
    let albedo = Vector3::new(1.0, 0.76, 0.33);
    let r_tex  = cm_texel_from_dir(&r);

    ( lookup_dir_cm  (&cm.cos_1  , &n)                                  * ldotn
    + lookup_texel_cm(&cm.cos_8  , r_tex) * normalize_phong_lobe(8.0  )
    + lookup_texel_cm(&cm.cos_512, r_tex) * normalize_phong_lobe(512.0) * (1.0 - ldotn)
    )
    * albedo * (*col * *col)
}

fn shader_cm_blue(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                  cm: &IrradianceCMSet) -> V3F {
    let n      = fast_normalize(n);
    let l      = fast_normalize(&(*eye.as_vector() - *p));
    let ldotn  = na::clamp(na::dot(&l, &n), 0.0, 1.0);
    let eye    = *p - *eye.as_vector();
    let r      = reflect(&eye, &n);
    let r_tex  = cm_texel_from_dir(&r);

    ( lookup_dir_cm  (&cm.cos_1  ,  &n)   * Vector3::new(0.2, 0.2, 0.8) * ldotn
    + lookup_texel_cm(&cm.cos_64 , r_tex) * normalize_phong_lobe(64.0 ) * 0.75
    + lookup_texel_cm(&cm.cos_512, r_tex) * normalize_phong_lobe(512.0) * (1.0 - ldotn)
    )
    * (*col * *col)
}

fn shader_cm_blinn_schlick(p: &V3F, n: &V3F, col: &V3F, eye: &P3F, _tick: f64,
                           cm: &IrradianceCMSet) -> V3F {
    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vector();
    let r   = reflect(&eye, &n);
    let h   = (n + r) / (n + r).norm();
    let w   = 1.0 - na::clamp(na::dot(&h, &eye), 0.0, 1.0);
    let w   = w * w;

    (  lookup_dir_cm(&cm.cos_1 , &n) * V3F::new(0.8, 0.65, 1.0) * w
    + (lookup_dir_cm(&cm.cos_64, &h) * normalize_phong_lobe(64.0) * (1.25 - w))
    )
    * (*col * *col)
}

fn fresnel_conductor(
    cosi: f32, // Cosine between normal and incident ray
    eta:  f32, // Index of refraction
    k:    f32) // Absorption coefficient
    -> f32 {
    // Compute Fresnel term for a conductor, PBRT 1st edition p422

    // Material | Eta   | K
    // ------------------------
    // Gold     | 0.370 | 2.820
    // Silver   | 0.177 | 3.638
    // Copper   | 0.617 | 2.63
    // Steel    | 2.485 | 3.433

    let tmp = (eta * eta + k * k) * cosi * cosi;
    let r_parallel_2 =
        (tmp - (2.0 * eta * cosi) + 1.0) /
        (tmp + (2.0 * eta * cosi) + 1.0);
    let tmp_f = eta * eta + k * k;
    let r_perpend_2 =
        (tmp_f - (2.0 * eta * cosi) + cosi * cosi) /
        (tmp_f + (2.0 * eta * cosi) + cosi * cosi);
    (r_parallel_2 + r_perpend_2) / 2.0
}

fn fast_unit_pow16(v: f32) -> f32 {
    // Fast X^16 function for X e [0, 1] using a 256 entry lookup table
    //
    // Table generation:
    //
    // for i in 600..256 + 600 {
    //     let v = i as f32 / (600.0 + 255.0);
    //     println!("{:<12},", v.powf(16.0));
    // }
    //
    // Table is shifted so more entries are used for the larger values, useful when
    // dealing with floats that will be converted to 8bit color values

    static TBL: [f32; 256] = [
        0.003459093 , 0.003552495 , 0.0036482627, 0.0037464416, 0.0038470984, 0.003950281 ,
        0.0040560593, 0.004164483 , 0.004275625 , 0.004389537 , 0.004506296 , 0.0046259556,
        0.004748595 , 0.0048742713, 0.005003067 , 0.005135042 , 0.005270282 , 0.005408848 ,
        0.0055508246, 0.0056962967, 0.0058453293, 0.005998019 , 0.006154434 , 0.006314675 ,
        0.0064788125, 0.006646952 , 0.006819167 , 0.0069955676, 0.0071762297, 0.0073612686,
        0.0075507634, 0.0077448343, 0.007943563 , 0.008147076 , 0.008355458 , 0.00856884  ,
        0.008787312 , 0.009010997 , 0.009240023 , 0.009474486 , 0.00971453  , 0.009960255 ,
        0.01021181  , 0.010469298 , 0.010732877 , 0.011002653 , 0.011278792 , 0.011561403 ,
        0.011850657 , 0.012146669 , 0.012449619 , 0.012759625 , 0.0130768735, 0.013401488 ,
        0.013733663 , 0.0140735265, 0.014421281 , 0.0147770615, 0.015141058 , 0.015513466 ,
        0.015894428 , 0.016284168 , 0.016682833 , 0.017090656 , 0.017507788 , 0.017934473 ,
        0.018370869 , 0.018817227 , 0.019273717 , 0.019740595 , 0.020218033 , 0.020706309 ,
        0.021205597 , 0.02171618  , 0.022238243 , 0.022772085 , 0.023317892 , 0.023875948 ,
        0.024446534 , 0.025029855 , 0.025626237 , 0.026235888 , 0.026859147 , 0.027496237 ,
        0.028147504 , 0.028813178 , 0.029493624 , 0.030189078 , 0.030899918 , 0.03162639  ,
        0.03236889  , 0.03312767  , 0.033903137 , 0.03469556  , 0.03550536  , 0.036332812 ,
        0.03717832  , 0.03804229  , 0.038925007 , 0.03982695  , 0.040748402 , 0.04168987  ,
        0.042651646 , 0.043634243 , 0.04463798  , 0.04566339  , 0.046710797 , 0.047780745 ,
        0.048873585 , 0.04998988  , 0.051129986 , 0.052294493 , 0.05348377  , 0.05469843  ,
        0.05593885  , 0.05720567  , 0.05849928  , 0.059820276 , 0.06116927  , 0.06254667  ,
        0.06395318  , 0.06538923  , 0.06685554  , 0.06835256  , 0.06988104  , 0.07144143  ,
        0.073034525 , 0.0746608   , 0.07632106  , 0.0780158   , 0.07974585  , 0.081511736 ,
        0.08331432  , 0.08515413  , 0.08703208  , 0.0889487   , 0.09090483  , 0.09290134  ,
        0.0949388   , 0.097018205 , 0.09914013  , 0.10130563  , 0.10351528  , 0.105770186 ,
        0.108070955 , 0.1104187   , 0.112814076 , 0.115258224 , 0.11775182  , 0.12029606  ,
        0.122891635 , 0.12553978  , 0.1282412   , 0.1309972   , 0.1338085   , 0.13667643  ,
        0.13960177  , 0.14258571  , 0.14562954  , 0.14873405  , 0.15190066  , 0.15513025  ,
        0.15842429  , 0.16178365  , 0.16520987  , 0.16870385  , 0.1722672   , 0.17590083  ,
        0.17960641  , 0.18338488  , 0.18723798  , 0.19116667  , 0.19517274  , 0.19925721  ,
        0.20342192  , 0.20766792  , 0.21199688  , 0.21641058  , 0.22091007  , 0.2254974   ,
        0.23017366  , 0.23494098  , 0.23980048  , 0.24475436  , 0.2498038   , 0.25495103  ,
        0.2601973   , 0.26554492  , 0.27099517  , 0.27655044  , 0.28221205  , 0.2879825   ,
        0.2938631   , 0.29985642  , 0.3059639   , 0.31218818  , 0.31853068  , 0.32499382  ,
        0.3315801   , 0.33829102  , 0.34512943  , 0.35209695  , 0.3591965   , 0.36642978  ,
        0.37379977  , 0.3813082   , 0.38895822  , 0.39675155  , 0.4046915   , 0.4127798   ,
        0.4210199   , 0.4294136   , 0.4379644   , 0.4466742   , 0.45554665  , 0.46458364  ,
        0.47378847  , 0.48316458  , 0.49271393  , 0.5024405   , 0.5123463   , 0.52243555  ,
        0.5327103   , 0.5431748   , 0.5538312   , 0.564684    , 0.5757353   , 0.58698964  ,
        0.59844947  , 0.61011934  , 0.6220017   , 0.6341014   , 0.64642084  , 0.65896505  ,
        0.67173654  , 0.6847404   , 0.6979794   , 0.711458    , 0.7251811   , 0.73915136  ,
        0.7533743   , 0.7678528   , 0.78259254  , 0.7975965   , 0.81287056  , 0.8284178   ,
        0.8442442   , 0.86035293  , 0.87675035  , 0.8934395   , 0.91042703  , 0.9277161   ,
        0.9453135   , 0.96322256  , 0.98145026  , 1.0
    ];

    let idx = (v * 855.0 - 600.0) as i32;
    if idx < 0 {
        0.0
    } else {
        if idx > 255 {
            1.0
        } else {
            unsafe { * TBL.get_unchecked(idx as usize) }
        }
    }
}

#[no_mangle]
pub extern fn rast_get_num_shaders() -> i32 { 16 }

#[no_mangle]
pub extern fn rast_get_shader_name(idx: i32) -> *const u8 { shader_by_idx(idx).0.as_ptr() }

fn shader_by_idx<'a>(idx: i32) -> (&'a str, bool, Shader) {
    // Retrieve shader name, cube map usage and function by its index

    let shaders: [(&str, bool, Shader); 16] = [
        // Null terminated names so we can easily pass them as C strings
        ("BakedColor\0"       , false, shader_color             ),
        ("Normals\0"          , false, shader_n_to_color        ),
        ("Headlight\0"        , false, shader_headlight         ),
        ("Plastic2xDirLight\0", false, shader_dir_light         ),
        ("CMDiffuse\0"        , true , shader_cm_diffuse        ),
        ("CMRefl\0"           , true , shader_cm_refl           ),
        ("CMCoated\0"         , true , shader_cm_coated         ),
        ("CMDiffRim\0"        , true , shader_cm_diff_rim       ),
        ("CMGlossy\0"         , true , shader_cm_glossy         ),
        ("CMGreenHighlight\0" , true , shader_cm_green_highlight),
        ("CMRedMaterial\0"    , true , shader_cm_red_material   ),
        ("CMMetallic\0"       , true , shader_cm_metallic       ),
        ("CMSuperShiny\0"     , true , shader_cm_super_shiny    ),
        ("CMGold\0"           , true , shader_cm_gold           ),
        ("CMBlue\0"           , true , shader_cm_blue           ),
        ("CMBlinnSchlick\0"   , true , shader_cm_blinn_schlick  )
    ];

    assert!(rast_get_num_shaders() as usize == shaders.len());
    if idx as usize >= shaders.len() {
        panic!("shader_by_idx: Invalid index: {}", idx)
    }

    shaders[idx as usize]
}

//
// ----------------------------------
// Vertex Processing & Transformation
// ----------------------------------
//

struct TransformedVertex {
    vp:    Point4<f32>, // Projected, perspective divided, viewport transformed vertex with W
                      // (note that we actually store 1/W in the last component)
    world: V3F,       // World space vertex and normal for lighting computations etc.
    n:     V3F,       // ...
    col:   V3F        // Color
}

fn transform_vertices(vtx_in: &[Vertex],
                      vtx_out: &mut [TransformedVertex],
                      normalize_mesh_dimensions: &Matrix4<f32>,
                      w: i32,
                      h: i32,
                      eye: &P3F) {
    // Build a mesh to viewport transformation and write out a transformed set of vertices

    // Build transformations (TODO: We do this for each chunk of vertices processed)
    let mesh_to_world       = *normalize_mesh_dimensions;
    let world_to_view       = look_at(eye, &Point3::new(0.0, 0.0, 0.0), &Vector3::y());
    let view_to_proj        = perspective(45.0, w as f32 / h as f32, 0.1, 10.0);
    let wh                  = w as f32 / 2.0;
    let hh                  = h as f32 / 2.0;
                              // TODO: We're applying the viewport transform before the
                              //       perspective divide, why does this actually work
                              //       identically to doing it right after it below?
    let proj_to_vp          = Matrix4::new(wh,  0.0, 0.0, wh,
                                           0.0, hh,  0.0, hh,
                                           0.0, 0.0, 1.0, 0.0,
                                           0.0, 0.0, 0.0, 1.0);
    let world_to_vp         = proj_to_vp * view_to_proj * world_to_view;
    let mesh_to_world_it_33 = na::from_homogeneous::<Matrix4<f32>, Matrix3<f32>>
                                  (&mesh_to_world.inverse().unwrap().transpose());

    // Transform and copy into target slice instead of copy and transform in-place
    for (src, dst) in vtx_in.iter().zip(vtx_out.iter_mut()) {
        // Transform from mesh into world space
        let world_h: Point4<f32> = mesh_to_world * na::to_homogeneous(&src.p);
        dst.world = Vector3::new(world_h.x, world_h.y, world_h.z);

        // World to viewport. Note that we do the perspective divide manually instead of using
        //   dst = na::from_homogeneous(&(transf * na::to_homogeneous(&src)));
        // so we can keep W around
        dst.vp    = world_to_vp * world_h;
        let inv_w = 1.0 / dst.vp.w;
        dst.vp.x *= inv_w;
        dst.vp.y *= inv_w;
        dst.vp.z *= inv_w;

        // We need 1/w for the perspective correct attribute interpolation,
        // just store the reciprocal we already computed
        dst.vp.w = inv_w;

        // Multiply with the 3x3 IT for world space normals
        dst.n = mesh_to_world_it_33 * src.n;

        // Copy color
        dst.col = src.col;
    }
}

// The camera related functions in nalgebra like Isometry3::look_at_z() and PerspMat3::new()
// are all using some rather unusual conventions and are not documented. Replace them with
// custom variants that work like the usual OpenGL style versions

fn look_at(eye: &P3F, at: &P3F, up: &V3F) -> Matrix4<f32> {
    let zaxis = na::normalize(&(*eye - *at));
    let xaxis = na::normalize(&na::cross(up, &zaxis));
    let yaxis = na::cross(&zaxis, &xaxis);

    Matrix4::new(xaxis.x, xaxis.y, xaxis.z, na::dot(&-eye.to_vector(), &xaxis),
                 yaxis.x, yaxis.y, yaxis.z, na::dot(&-eye.to_vector(), &yaxis),
                 zaxis.x, zaxis.y, zaxis.z, na::dot(&-eye.to_vector(), &zaxis),
                 0.0,     0.0,     0.0,     1.0)
}

fn perspective(fovy_deg: f32, aspect: f32, near: f32, far: f32) -> Matrix4<f32> {
    let tan_half_fovy = (deg_to_rad(fovy_deg) / 2.0).tan();
    let m00 = 1.0 / (aspect * tan_half_fovy);
    let m11 = 1.0 / tan_half_fovy;
    let m22 = -(far + near) / (far - near);
    let m23 = -(2.0 * far * near) / (far - near);
    let m32 = -1.0;

    Matrix4::new(m00, 0.0, 0.0, 0.0,
                 0.0, m11, 0.0, 0.0,
                 0.0, 0.0, m22, m23,
                 0.0, 0.0, m32, 0.0)
}

//
// ---------------------
// Miscellaneous Drawing
// ---------------------
//

#[no_mangle]
pub extern fn rast_get_num_backgrounds() -> i32 { 5 }

fn draw_bg_gradient(bg_idx: i32, w: i32, h: i32, fb: *mut u32) {
    // Fill the framebuffer with a vertical gradient

    let start;
    let end;
    match bg_idx {
        0 => { start = Vector3::new(0.3, 0.3, 0.3); end = Vector3::new(0.7, 0.7, 0.7); }
        1 => { start = Vector3::new(1.0, 0.4, 0.0); end = Vector3::new(0.0, 0.5, 0.5); }
        2 => { start = Vector3::new(1.0, 0.0, 1.0); end = Vector3::new(1.0, 0.0, 1.0); }
        3 => { start = Vector3::new(1.0, 1.0, 1.0); end = Vector3::new(1.0, 1.0, 1.0); }
        4 => { start = Vector3::new(0.0, 0.0, 0.0); end = Vector3::new(0.0, 0.0, 0.0); }
        _ => panic!("draw_bg_gradient: Invalid index: {}", bg_idx)
    }

    for y in 0..h {
        let pos = y as f32 / (h - 1) as f32;
        let col = start * (1.0 - pos) + end * pos;

        // No gamma. The gradients look nice without it, as it's more perceptually linear
        // this way. It's also faster
        let col32 = rgbf_to_abgr32(col.x, col.y, col.z);

        let fb_row = unsafe { fb.offset((y * w) as isize) };
        for x in 0..w {
            unsafe {
                * fb_row.offset(x as isize) = col32;
            }
        }
    }
}

fn draw_line(x1: f32, y1: f32, x2: f32, y2: f32, fb: *mut u32, w: i32, h: i32) {
    // Draw a line using the DDA algorithm

    // Just so edges with same vertices but different winding get the same coordinates
    let (x1, y1, x2, y2) = if x2 > x1 { (x1, y1, x2, y2) } else { (x2, y2, x1, y1) };

    let dx    = x2 - x1;
    let dy    = y2 - y1;
    let s     = if dx.abs() > dy.abs() { dx.abs() } else { dy.abs() };
    let xi    = dx / s;
    let yi    = dy / s;
    let mut x = x1;
    let mut y = y1;
    let mut m = 0.0;

    while m < s {
        let xr = x as i32;
        let yr = y as i32;

        if xr >= 0 && xr < w && yr >= 0 && yr < h {
            let idx = xr + yr * w;
            unsafe { * fb.offset(idx as isize) = 0x00FFFFFF }
        }

        x += xi;
        y += yi;
        m += 1.0;
    }
}

//
// ------------------
// Framebuffer Output
// ------------------
//

fn rgbf_to_abgr32(r: f32, g: f32, b: f32) -> u32 {
    // Clamp and covert floating-point RGB triplet to a packed ABGR32, no gamma correction

    let r8 = (na::clamp(r, 0.0, 1.0) * 255.0) as u32;
    let g8 = (na::clamp(g, 0.0, 1.0) * 255.0) as u32;
    let b8 = (na::clamp(b, 0.0, 1.0) * 255.0) as u32;

    r8 | (g8 << 8) | (b8 << 16)
}

fn rgbf_to_abgr32_gamma(r: f32, g: f32, b: f32) -> u32 {
    // Clamp, gamma correct and covert floating-point RGB triplet to a packed ABGR32
    // value. Doing gamma correction in full float precision is very expensive due to the
    // pow() calls. 8bit introduces severe banding. A 11bit LUT is a good compromise

    let r11_idx = (r * 2047.0) as i32;
    let g11_idx = (g * 2047.0) as i32;
    let b11_idx = (b * 2047.0) as i32;

    let r8 = if r11_idx < 0 {
        0
    } else {
        if r11_idx > 2047 {
            255
        } else {
            unsafe { * GAMMA_11BIT_LUT.get_unchecked(r11_idx as usize) as u32 }
        }
    };

    let g8 = if g11_idx < 0 {
        0
    } else {
        if g11_idx > 2047 {
            255
        } else {
            unsafe { * GAMMA_11BIT_LUT.get_unchecked(g11_idx as usize) as u32 }
        }
    };

    let b8 = if r11_idx < 0 {
        0
    } else {
        if b11_idx > 2047 {
            255
        } else {
            unsafe { * GAMMA_11BIT_LUT.get_unchecked(b11_idx as usize) as u32 }
        }
    };

    r8 | (g8 << 8) | (b8 << 16)
}

// 11bit gamma 2.2 correction LUT, fits in 2KB and just about good enough
//
// for i in 0..2048  {
//     println!("{:<3},", ((i as f32 / 2047.0).powf(1.0 / 2.2) * 255.0).round() as u8);
// }

static GAMMA_11BIT_LUT: [u8; 2048] = [
    0  , 8  , 11 , 13 , 15 , 17 , 18 , 19 , 21 , 22 , 23 , 24 , 25 , 26 , 26 , 27 , 28 , 29 ,
    30 , 30 , 31 , 32 , 32 , 33 , 34 , 34 , 35 , 36 , 36 , 37 , 37 , 38 , 39 , 39 , 40 , 40 ,
    41 , 41 , 42 , 42 , 43 , 43 , 44 , 44 , 45 , 45 , 45 , 46 , 46 , 47 , 47 , 48 , 48 , 48 ,
    49 , 49 , 50 , 50 , 50 , 51 , 51 , 52 , 52 , 52 , 53 , 53 , 54 , 54 , 54 , 55 , 55 , 55 ,
    56 , 56 , 56 , 57 , 57 , 57 , 58 , 58 , 58 , 59 , 59 , 59 , 60 , 60 , 60 , 61 , 61 , 61 ,
    62 , 62 , 62 , 63 , 63 , 63 , 63 , 64 , 64 , 64 , 65 , 65 , 65 , 66 , 66 , 66 , 66 , 67 ,
    67 , 67 , 68 , 68 , 68 , 68 , 69 , 69 , 69 , 69 , 70 , 70 , 70 , 71 , 71 , 71 , 71 , 72 ,
    72 , 72 , 72 , 73 , 73 , 73 , 73 , 74 , 74 , 74 , 74 , 75 , 75 , 75 , 75 , 76 , 76 , 76 ,
    76 , 77 , 77 , 77 , 77 , 77 , 78 , 78 , 78 , 78 , 79 , 79 , 79 , 79 , 80 , 80 , 80 , 80 ,
    81 , 81 , 81 , 81 , 81 , 82 , 82 , 82 , 82 , 83 , 83 , 83 , 83 , 83 , 84 , 84 , 84 , 84 ,
    84 , 85 , 85 , 85 , 85 , 86 , 86 , 86 , 86 , 86 , 87 , 87 , 87 , 87 , 87 , 88 , 88 , 88 ,
    88 , 88 , 89 , 89 , 89 , 89 , 89 , 90 , 90 , 90 , 90 , 90 , 91 , 91 , 91 , 91 , 91 , 92 ,
    92 , 92 , 92 , 92 , 93 , 93 , 93 , 93 , 93 , 93 , 94 , 94 , 94 , 94 , 94 , 95 , 95 , 95 ,
    95 , 95 , 96 , 96 , 96 , 96 , 96 , 96 , 97 , 97 , 97 , 97 , 97 , 98 , 98 , 98 , 98 , 98 ,
    98 , 99 , 99 , 99 , 99 , 99 , 99 , 100, 100, 100, 100, 100, 101, 101, 101, 101, 101, 101,
    102, 102, 102, 102, 102, 102, 103, 103, 103, 103, 103, 103, 104, 104, 104, 104, 104, 104,
    105, 105, 105, 105, 105, 105, 106, 106, 106, 106, 106, 106, 107, 107, 107, 107, 107, 107,
    107, 108, 108, 108, 108, 108, 108, 109, 109, 109, 109, 109, 109, 110, 110, 110, 110, 110,
    110, 110, 111, 111, 111, 111, 111, 111, 112, 112, 112, 112, 112, 112, 112, 113, 113, 113,
    113, 113, 113, 114, 114, 114, 114, 114, 114, 114, 115, 115, 115, 115, 115, 115, 115, 116,
    116, 116, 116, 116, 116, 116, 117, 117, 117, 117, 117, 117, 117, 118, 118, 118, 118, 118,
    118, 118, 119, 119, 119, 119, 119, 119, 119, 120, 120, 120, 120, 120, 120, 120, 121, 121,
    121, 121, 121, 121, 121, 122, 122, 122, 122, 122, 122, 122, 123, 123, 123, 123, 123, 123,
    123, 123, 124, 124, 124, 124, 124, 124, 124, 125, 125, 125, 125, 125, 125, 125, 125, 126,
    126, 126, 126, 126, 126, 126, 127, 127, 127, 127, 127, 127, 127, 127, 128, 128, 128, 128,
    128, 128, 128, 128, 129, 129, 129, 129, 129, 129, 129, 129, 130, 130, 130, 130, 130, 130,
    130, 131, 131, 131, 131, 131, 131, 131, 131, 132, 132, 132, 132, 132, 132, 132, 132, 133,
    133, 133, 133, 133, 133, 133, 133, 134, 134, 134, 134, 134, 134, 134, 134, 134, 135, 135,
    135, 135, 135, 135, 135, 135, 136, 136, 136, 136, 136, 136, 136, 136, 137, 137, 137, 137,
    137, 137, 137, 137, 137, 138, 138, 138, 138, 138, 138, 138, 138, 139, 139, 139, 139, 139,
    139, 139, 139, 140, 140, 140, 140, 140, 140, 140, 140, 140, 141, 141, 141, 141, 141, 141,
    141, 141, 141, 142, 142, 142, 142, 142, 142, 142, 142, 142, 143, 143, 143, 143, 143, 143,
    143, 143, 144, 144, 144, 144, 144, 144, 144, 144, 144, 145, 145, 145, 145, 145, 145, 145,
    145, 145, 146, 146, 146, 146, 146, 146, 146, 146, 146, 147, 147, 147, 147, 147, 147, 147,
    147, 147, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 149, 149, 149, 149, 149, 149,
    149, 149, 149, 150, 150, 150, 150, 150, 150, 150, 150, 150, 151, 151, 151, 151, 151, 151,
    151, 151, 151, 151, 152, 152, 152, 152, 152, 152, 152, 152, 152, 153, 153, 153, 153, 153,
    153, 153, 153, 153, 153, 154, 154, 154, 154, 154, 154, 154, 154, 154, 155, 155, 155, 155,
    155, 155, 155, 155, 155, 155, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 157, 157,
    157, 157, 157, 157, 157, 157, 157, 157, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158,
    159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 160, 160, 160, 160, 160, 160, 160, 160,
    160, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 162, 162, 162, 162, 162, 162,
    162, 162, 162, 162, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 164, 164, 164, 164,
    164, 164, 164, 164, 164, 164, 164, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 166,
    166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 167, 167, 167, 167, 167, 167, 167, 167,
    167, 167, 167, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 171,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 172, 172, 172, 172, 172, 172, 172, 172,
    172, 172, 172, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 177, 177, 177, 177, 177, 177,
    177, 177, 177, 177, 177, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 179, 179,
    179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 180, 180, 180, 180, 180, 180, 180, 180,
    180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 182, 182, 182,
    182, 182, 182, 182, 182, 182, 182, 182, 182, 183, 183, 183, 183, 183, 183, 183, 183, 183,
    183, 183, 183, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 185, 185, 185,
    185, 185, 185, 185, 185, 185, 185, 185, 185, 186, 186, 186, 186, 186, 186, 186, 186, 186,
    186, 186, 186, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 188, 188, 188,
    188, 188, 188, 188, 188, 188, 188, 188, 188, 189, 189, 189, 189, 189, 189, 189, 189, 189,
    189, 189, 189, 189, 190, 190, 190, 190, 190, 190, 190, 190, 190, 190, 190, 190, 191, 191,
    191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 192, 192, 192, 192, 192, 192, 192,
    192, 192, 192, 192, 192, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193,
    194, 194, 194, 194, 194, 194, 194, 194, 194, 194, 194, 194, 194, 195, 195, 195, 195, 195,
    195, 195, 195, 195, 195, 195, 195, 196, 196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
    196, 196, 197, 197, 197, 197, 197, 197, 197, 197, 197, 197, 197, 197, 197, 198, 198, 198,
    198, 198, 198, 198, 198, 198, 198, 198, 198, 198, 199, 199, 199, 199, 199, 199, 199, 199,
    199, 199, 199, 199, 199, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200,
    200, 201, 201, 201, 201, 201, 201, 201, 201, 201, 201, 201, 201, 201, 202, 202, 202, 202,
    202, 202, 202, 202, 202, 202, 202, 202, 202, 203, 203, 203, 203, 203, 203, 203, 203, 203,
    203, 203, 203, 203, 203, 204, 204, 204, 204, 204, 204, 204, 204, 204, 204, 204, 204, 204,
    205, 205, 205, 205, 205, 205, 205, 205, 205, 205, 205, 205, 205, 205, 206, 206, 206, 206,
    206, 206, 206, 206, 206, 206, 206, 206, 206, 207, 207, 207, 207, 207, 207, 207, 207, 207,
    207, 207, 207, 207, 207, 208, 208, 208, 208, 208, 208, 208, 208, 208, 208, 208, 208, 208,
    208, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209, 210, 210, 210,
    210, 210, 210, 210, 210, 210, 210, 210, 210, 210, 210, 211, 211, 211, 211, 211, 211, 211,
    211, 211, 211, 211, 211, 211, 211, 212, 212, 212, 212, 212, 212, 212, 212, 212, 212, 212,
    212, 212, 212, 213, 213, 213, 213, 213, 213, 213, 213, 213, 213, 213, 213, 213, 213, 214,
    214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 214, 215, 215, 215, 215,
    215, 215, 215, 215, 215, 215, 215, 215, 215, 215, 216, 216, 216, 216, 216, 216, 216, 216,
    216, 216, 216, 216, 216, 216, 216, 217, 217, 217, 217, 217, 217, 217, 217, 217, 217, 217,
    217, 217, 217, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218,
    219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 220, 220, 220, 220,
    220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 221, 221, 221, 221, 221, 221, 221,
    221, 221, 221, 221, 221, 221, 221, 221, 222, 222, 222, 222, 222, 222, 222, 222, 222, 222,
    222, 222, 222, 222, 222, 223, 223, 223, 223, 223, 223, 223, 223, 223, 223, 223, 223, 223,
    223, 223, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 225,
    225, 225, 225, 225, 225, 225, 225, 225, 225, 225, 225, 225, 225, 225, 226, 226, 226, 226,
    226, 226, 226, 226, 226, 226, 226, 226, 226, 226, 226, 226, 227, 227, 227, 227, 227, 227,
    227, 227, 227, 227, 227, 227, 227, 227, 227, 228, 228, 228, 228, 228, 228, 228, 228, 228,
    228, 228, 228, 228, 228, 228, 229, 229, 229, 229, 229, 229, 229, 229, 229, 229, 229, 229,
    229, 229, 229, 229, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230,
    230, 230, 231, 231, 231, 231, 231, 231, 231, 231, 231, 231, 231, 231, 231, 231, 231, 232,
    232, 232, 232, 232, 232, 232, 232, 232, 232, 232, 232, 232, 232, 232, 232, 233, 233, 233,
    233, 233, 233, 233, 233, 233, 233, 233, 233, 233, 233, 233, 233, 234, 234, 234, 234, 234,
    234, 234, 234, 234, 234, 234, 234, 234, 234, 234, 234, 235, 235, 235, 235, 235, 235, 235,
    235, 235, 235, 235, 235, 235, 235, 235, 235, 236, 236, 236, 236, 236, 236, 236, 236, 236,
    236, 236, 236, 236, 236, 236, 236, 237, 237, 237, 237, 237, 237, 237, 237, 237, 237, 237,
    237, 237, 237, 237, 237, 238, 238, 238, 238, 238, 238, 238, 238, 238, 238, 238, 238, 238,
    238, 238, 238, 239, 239, 239, 239, 239, 239, 239, 239, 239, 239, 239, 239, 239, 239, 239,
    239, 239, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240,
    241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 242,
    242, 242, 242, 242, 242, 242, 242, 242, 242, 242, 242, 242, 242, 242, 242, 243, 243, 243,
    243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 244, 244, 244, 244,
    244, 244, 244, 244, 244, 244, 244, 244, 244, 244, 244, 244, 244, 245, 245, 245, 245, 245,
    245, 245, 245, 245, 245, 245, 245, 245, 245, 245, 245, 246, 246, 246, 246, 246, 246, 246,
    246, 246, 246, 246, 246, 246, 246, 246, 246, 246, 247, 247, 247, 247, 247, 247, 247, 247,
    247, 247, 247, 247, 247, 247, 247, 247, 247, 248, 248, 248, 248, 248, 248, 248, 248, 248,
    248, 248, 248, 248, 248, 248, 248, 248, 249, 249, 249, 249, 249, 249, 249, 249, 249, 249,
    249, 249, 249, 249, 249, 249, 249, 249, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
    250, 250, 250, 250, 250, 250, 250, 251, 251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
    251, 251, 251, 251, 251, 251, 252, 252, 252, 252, 252, 252, 252, 252, 252, 252, 252, 252,
    252, 252, 252, 252, 252, 252, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253,
    253, 253, 253, 253, 253, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254,
    254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255
];

//
// ----------
// Rasterizer
// ----------
//

macro_rules! mk_rasterizer {
  ($shade_per_pixel: expr, $rast_fn_name: ident) => {
    #[inline(always)]
    fn $rast_fn_name(// Triangle
                     vtx0: &TransformedVertex,
                     vtx1: &TransformedVertex,
                     vtx2: &TransformedVertex,
                     // Parameters for shading
                     shader: &Shader,
                     eye: &P3F,
                     tick: f64,
                     cm: &IrradianceCMSet,
                     // Active tile / scissor rect
                     tx1: i32,
                     ty1: i32,
                     tx2: i32,
                     ty2: i32,
                     // Frame and depth buffer
                     fb_stride: i32,
                     fb: *mut u32,
                     depth_ptr: *mut f32) {
        // TODO: This code would really benefit from SIMD intrinsics

        // Break out positions (viewport and world), colors and normals
        let v0 = vtx0.vp; let p0 = vtx0.world; let c0 = vtx0.col; let n0 = vtx0.n;
        let v1 = vtx1.vp; let p1 = vtx1.world; let c1 = vtx1.col; let n1 = vtx1.n;
        let v2 = vtx2.vp; let p2 = vtx2.world; let c2 = vtx2.col; let n2 = vtx2.n;

        // Convert to 28.4 fixed-point. It would be most accurate to round() on these values,
        // but that is extremely slow. Truncate will do, we're at most a sub-pixel off
        let x0 = (v0.x * 16.0) as i32;
        let y0 = (v0.y * 16.0) as i32;
        let x1 = (v1.x * 16.0) as i32;
        let y1 = (v1.y * 16.0) as i32;
        let x2 = (v2.x * 16.0) as i32;
        let y2 = (v2.y * 16.0) as i32;

        // Edge deltas
        let dx10 = x1 - x0; let dy01 = y0 - y1;
        let dx21 = x2 - x1; let dy12 = y1 - y2;
        let dx02 = x0 - x2; let dy20 = y2 - y0;

        // Backface culling through cross product. The Z component of the resulting vector
        // tells us if the triangle is facing the camera or not, its magnitude is the 2x the
        // signed area of the triangle, which is exactly what we need to normalize our
        // barycentric coordinates later
        let tri_a2 = (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0);
        if tri_a2 <= 0 { return }
        let inv_tri_a2 = 1.0 / tri_a2 as f32;

        // We test triangle coverage at integer coordinates D3D9 style vs centers like >=D3D10
        // and OpenGL. Our pixel center is at the bottom-left. We also use a bottom-left fill
        // convention, unlike the more conventional top-left one. It's what D3D does, but with
        // the Y axis flipped (our origin is bottom-left). We normally consider only raster
        // positions as on the inside of an edge if the half-space function returns a positive
        // value. For the bottom-left edges we want the contested raster positions lying on
        // the edge to belong to its triangle.
        //
        // Consider these two 2x2 triangulated quads and how they'd rasterize with each convention
        //
        // *--*--*    top-left    bottom-left
        // |2   /|
        // *  *  *      2 2           . .
        // |/   3|      2 3           2 3
        // *--*--*      0 0           3 3
        // |0   /|      0 1           0 1
        // *  *  *      . .           1 1
        // |/   1|
        // *--*--*
        //
        // With a bottom-left coordinate system origin and pixel center the latter fill convention
        // just makes more sense to me. No need to shift our AABB's Y by one and we can just round
        // up on both min / max bound

        // AABB of the triangle, map to pixels by rounding up
        let min_x = (min3(x0, x1, x2) + 0xF) >> 4;
        let min_y = (min3(y0, y1, y2) + 0xF) >> 4;
        let max_x = (max3(x0, x1, x2) + 0xF) >> 4;
        let max_y = (max3(y0, y1, y2) + 0xF) >> 4;

        // Clip against tile (which we assume to be inside the framebuffer)
        let min_x = cmp::max(min_x, tx1);
        let min_y = cmp::max(min_y, ty1);
        let max_x = cmp::min(max_x, tx2);
        let max_y = cmp::min(max_y, ty2);

        // Early reject for triangles which are outside the tile
        if max_x <= min_x || max_y <= min_y { return }

        // Implement bottom-left fill convention. Classifying those edges is simple, as with
        // CCW vertex order they are either descending or horizontally moving left to right.
        // We basically want to turn the '> 0' comparison into '>= 0', and we do this by adding
        // a constant 1 for the half-space functions of those edges
        let e0add = if dy01 > 0 || (dy01 == 0 && dx10 > 0) { 1 } else { 0 };
        let e1add = if dy12 > 0 || (dy12 == 0 && dx21 > 0) { 1 } else { 0 };
        let e2add = if dy20 > 0 || (dy20 == 0 && dx02 > 0) { 1 } else { 0 };

        // We take the obvious formulation of the cross product edge functions
        //
        // hs0 = (x1 - x0) * (yf - y0) - (y1 - y0) * (xf - x0)
        // hs1 = (x2 - x1) * (yf - y1) - (y2 - y1) * (xf - x1)
        // hs2 = (x0 - x2) * (yf - y2) - (y0 - y2) * (xf - x2)
        //
        // and transform it into something more easily split based on its dependencies
        //
        // hs0 = (y0 - y1) * xf + (x1 - x0) * yf + (x0 * y1 - y0 * x1)
        // hs1 = (y1 - y2) * xf + (x2 - x1) * yf + (x1 * y2 - y1 * x2)
        // hs2 = (y2 - y0) * xf + (x0 - x2) * yf + (x2 * y0 - y2 * x0)
        //
        // Now we can separate the constant part and split the x/y dependent terms into an
        // initial value and one to add for every step in each loop direction

        // Edge function constant. The '+ 1' is so we can change the inside-edge compare in
        // the inner loop from > to >=, meaning we can just OR the signs of the edge functions
        let e0c = x0 * y1 - y0 * x1 + e0add + 1;
        let e1c = x1 * y2 - y1 * x2 + e1add + 1;
        let e2c = x2 * y0 - y2 * x0 + e2add + 1;

        // Starting value at AABB origin
        let mut e0y = dy01 * (min_x << 4) + dx10 * (min_y << 4) + e0c;
        let mut e1y = dy12 * (min_x << 4) + dx21 * (min_y << 4) + e1c;
        let mut e2y = dy20 * (min_x << 4) + dx02 * (min_y << 4) + e2c;

        // Fixed-point edge deltas (another shift because each pixel step is
        // 1 << 4 steps for the edge function)
        let fp_dx10 = dx10 << 4; let fp_dy01 = dy01 << 4; let fp_dx21 = dx21 << 4;
        let fp_dy12 = dy12 << 4; let fp_dx02 = dx02 << 4; let fp_dy20 = dy20 << 4;

        // During vertex processing we already replaced w with 1/w
        let inv_w_0 = v0.w;
        let inv_w_1 = v1.w;
        let inv_w_2 = v2.w;

        // Setup deltas for interpolating z, w and c with
        //
        // v0 + (v1 - v0) * b2 + (v2 - v0) * b0
        //
        // instead of
        //
        // v0 * b1 + v1 * b2 + v2 * b0
        let z10 = v1.z - v0.z;
        let z20 = v2.z - v0.z;
        let w10 = inv_w_1 - inv_w_0;
        let w20 = inv_w_2 - inv_w_0;
        let c10 = c1 * inv_w_1 - c0 * inv_w_0;
        let c20 = c2 * inv_w_2 - c0 * inv_w_0;

        for y in min_y..max_y {
            // Starting point for X stepping
            //
            // TODO: We could move the starting point to the intersection with the left edges,
            //       turning this a bit into a scanline rasterizer
            let mut e0x = e0y;
            let mut e1x = e1y;
            let mut e2x = e2y;

            let idx_y = y * fb_stride;
            let mut inside = false;

            for x in min_x..max_x {
                // Check the half-space functions for all three edges to see if we're inside
                // the triangle. These functions are basically just a cross product between an
                // edge and a vector from the current raster position to the edge. The resulting
                // vector will either point into or out of the screen, so we can check which
                // side of the edge we're on by the sign of the Z component. See notes for
                // 'e[012]c' as for how we do the compare
                if e0x | e1x | e2x >= 0 {
                    inside = true;

                    // The cross product from the edge function not only tells us which side
                    // we're on, but also the area of parallelogram formed by the two vectors.
                    // We're basically getting twice the area of one of the three triangles
                    // splitting the original triangle around the raster position. Those are
                    // already barycentric coordinates, we just need to normalize them with
                    // the triangle area we already computed with the cross product for the
                    // backface culling test. Don't forget to remove the fill convention
                    // (e[012]add) and comparison (+1) bias applied earlier
                    let b0 = (e0x - e0add - 1) as f32 * inv_tri_a2;
                    let b1 = (e1x - e1add - 1) as f32 * inv_tri_a2;
                    let b2 = (e2x - e2add - 1) as f32 * inv_tri_a2;

                    let idx = (x + idx_y) as isize;

                    // Interpolate and test depth. Note that we are interpolating z/w, which
                    // is linear in screen space, no special perspective correct interpolation
                    // required. We also use a Z buffer, not a W buffer
                    let z = v0.z + z10 * b2 + z20 * b0;
                    let d = unsafe { depth_ptr.offset(idx) };
                    if unsafe { *d > z } {
                        // Write depth
                        unsafe { *d = z };

                        // To do perspective correct interpolation of attributes we need
                        // to know w at the current raster position. We can compute it by
                        // interpolating 1/w linearly and then taking the reciprocal
                        let w_raster = 1.0 / (inv_w_0 + w10 * b2 + w20 * b0);

                        // Interpolate color. Perspective correct interpolation requires us to
                        // linearly interpolate col/w and then multiply by w
                        let c_raster = (c0  * inv_w_0 +
                                        c10 * b2      +
                                        c20 * b0) * w_raster;

                        // Shading
                        let out = if $shade_per_pixel {
                            // TODO: Also use faster 2xMAD form of barycentric interpolation for
                            //       the p/n attributes, just a bit tricky to get the delta setup
                            //       out of the per-vertex shading branch

                            // Also do perspective correct interpolation of the vertex normal
                            // and world space position, the shader might want these
                            let p_raster = (p0 * inv_w_0 * b1 +
                                            p1 * inv_w_1 * b2 +
                                            p2 * inv_w_2 * b0) * w_raster;
                            let n_raster = (n0 * inv_w_0 * b1 +
                                            n1 * inv_w_1 * b2 +
                                            n2 * inv_w_2 * b0) * w_raster;

                            // Call shader
                            //
                            // TODO: We could just generate one rasterizer instance for each shader
                            //       or alternatively switch to a deferred shading approach
                            shader(&p_raster, &n_raster, &c_raster, &eye, tick, cm)
                        } else {
                            // Just write interpolated per-vertex shading result
                            c_raster
                        };

                        // Gamma correct and write color
                        unsafe {
                            * fb.offset(idx) = rgbf_to_abgr32_gamma(out.x, out.y, out.z);
                        }
                    }
                } else {
                    // Very basic traversal optimization. Once we're inside the triangle, we can
                    // stop with the current row if we find ourselves outside again
                    if inside { break; }
                }

                // Step X
                e0x += fp_dy01;
                e1x += fp_dy12;
                e2x += fp_dy20;
            }

            // Step Y
            e0y += fp_dx10;
            e1y += fp_dx21;
            e2y += fp_dx02;
        }
    }
  };
}

mk_rasterizer!(true , rasterize_and_shade_triangle_pixel );
mk_rasterizer!(false, rasterize_and_shade_triangle_vertex);

//
// ------------
// Entry Points
// ------------
//

#[no_mangle]
pub extern fn rast_benchmark() {
    // Allocate framebuffer
    let w = 512;
    let h = 512;
    let mut fb: Vec<u32> = Vec::new();
    fb.resize((w * h) as usize, 0);
    let fb_ptr = fb.as_mut_ptr();

    // Benchmark name, reference speed and function
    let benchmarks:[(&str, i64, &Fn() -> ()); 12] = [
        ("KillerooV"  , 1812, &|| rast_draw(0, RenderMode::Fill, 0 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("HeadV"      , 2500, &|| rast_draw(0, RenderMode::Fill, 1 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("HandV"      ,  910, &|| rast_draw(0, RenderMode::Fill, 4 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("TorusKnotV" , 1287, &|| rast_draw(0, RenderMode::Fill, 6 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("CubeV"      , 1107, &|| rast_draw(0, RenderMode::Fill, 9 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("CornellBoxV", 1326, &|| rast_draw(0, RenderMode::Fill, 11, 5, 0, 0, 0., w, h, fb_ptr)),
        ("KillerooP"  , 2435, &|| rast_draw(1, RenderMode::Fill, 0 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("HeadP"      , 3841, &|| rast_draw(1, RenderMode::Fill, 1 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("HandP"      , 1689, &|| rast_draw(1, RenderMode::Fill, 4 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("TorusKnotP" , 3132, &|| rast_draw(1, RenderMode::Fill, 6 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("CubeP"      , 3461, &|| rast_draw(1, RenderMode::Fill, 9 , 5, 0, 0, 0., w, h, fb_ptr)),
        ("CornellBoxP", 3786, &|| rast_draw(1, RenderMode::Fill, 11, 5, 0, 0, 0., w, h, fb_ptr))
    ];

    // Run once so all the one-time initialization etc. is done
    for i in 0..benchmarks.len() { benchmarks[i].2(); }

    // Vector storing the best time
    let mut timings: Vec<i64> = Vec::new();
    timings.resize(benchmarks.len(), i64::MAX);

    // Run all benchmarks multiple times
    let num_runs = 40;
    for _ in 0..num_runs  {
        for i in 0..benchmarks.len() {
            // Measure and run
            let bench_fun = benchmarks[i].2;
            let start = PreciseTime::now();
            bench_fun();
            let end = PreciseTime::now();

            // Best time?
            timings[i] = cmp::min(timings[i], start.to(end).num_microseconds().unwrap());
        }
    }

    // Overall time
    let mut total_ref = 0;
    let mut total_now = 0;
    for i in 0..benchmarks.len() {
        total_ref += benchmarks[i].1;
        total_now += timings[i];
    }

    // Benchmark table
    let hr = "-------------------------------------------------";
    println!("\n      Name      |    Ref   |    Now   |  %-Diff");
    println!("{}", hr);
    for i in 0..benchmarks.len() + 1 {
        let name;
        let time_ref;
        let time_now;
        let time_diff;
        let percent_diff;

        // Print summary as last entry
        if i == benchmarks.len() {
            name         = "<Total>";
            time_ref     = total_ref;
            time_now     = total_now;
            time_diff    = total_now - total_ref;
            percent_diff = (time_diff as f64 / total_ref as f64) * 100.0;
            println!("{}", hr);
        } else {
            name         = benchmarks[i].0;
            time_ref     = benchmarks[i].1;
            time_now     = timings[i];
            time_diff    = time_now - time_ref;
            percent_diff = (time_diff as f64 / time_ref as f64) * 100.0;
        }

        let neutral     = ansi_term::Colour::White.dimmed();
        let regression  = ansi_term::Colour::Red  .normal();
        let improvement = ansi_term::Colour::Green.normal();
        let tolerance   = 1.0;
        let style       = if percent_diff <= -tolerance {
                              improvement
                          } else if percent_diff >= tolerance {
                              regression
                          } else {
                              neutral
                          };
        let display_str = format!("{:^16}|{:^7}μs |{:^7}μs | {:^+7.2}%",
                                  name,
                                  time_ref,
                                  time_now,
                                  percent_diff);

        println!("{}", style.paint(display_str));

    }
    println!("");
}

#[repr(i32)]
#[derive(PartialEq, Copy, Clone)]
pub enum RenderMode { Point, Line, Fill }

#[no_mangle]
pub extern fn rast_draw(shade_per_pixel: i32,
                        mode: RenderMode,
                        mesh_idx: i32,
                        shader_idx: i32,
                        env_map_idx: i32,
                        bg_idx: i32,
                        tick: f64,
                        w: i32,
                        h: i32,
                        fb: *mut u32) {
    // Transform, rasterize and shade mesh

    // Avoid passing a bool over the FFI, convert now
    let shade_per_pixel: bool = shade_per_pixel != 0;

    // let tick: f64 = 0.0;

    // Scene (mesh, camera, shader, environment)
    let (_, camera, mesh)    = mesh_by_idx(mesh_idx);
    let eye                  = camera(tick);
    let (_, show_cm, shader) = shader_by_idx(shader_idx);
    let (_, cm)              = cm_set_by_idx(env_map_idx);

    // Number of threads we're supposed to use
    lazy_static! {
        static ref NUM_THREADS: usize = {
            // Vertex processing seems to be slightly faster if we have one thread for each
            // physical core, rasterization / shading benefits strongly from HT. Use the full
            // thread count for now
            num_cpus::get()
        };
    }

    // Reusable thread pool
    struct SyncPool {
        cell: UnsafeCell<scoped_threadpool::Pool>
    }
    unsafe impl Sync for SyncPool { }
    lazy_static! {
        static ref POOL: SyncPool = {
            let pool = scoped_threadpool::Pool::new(*NUM_THREADS as u32);
            SyncPool { cell: UnsafeCell::new(pool) }
        };
    }
    let mut pool = unsafe { &mut *POOL.cell.get() };

    // Depth buffer
    let mut depth: Vec<f32> = Vec::new();

    // Prepare output buffer for transformed vertices
    let mut vtx_transf: Vec<TransformedVertex> = Vec::with_capacity(mesh.vtx.len());
    unsafe { vtx_transf.set_len(mesh.vtx.len()); }

    {
        // This is rather awkward, but we do gradient filling and depth clearing while we wait
        // for the vertex processing worker threads. Gives a small speedup
        let mut bg_and_depth = || {
            // Fill the framebuffer with a gradient or solid color
            draw_bg_gradient(bg_idx, w, h, fb);

            // Allocate and clear depth buffer
            if mode == RenderMode::Fill {
                depth.resize((w * h) as usize, 1.0);
            }
        };

        // Transform and shade vertices. Only pick parallel processing if we
        // actually have a significant vertex processing workload
        let do_vtx_shading = !shade_per_pixel && mode == RenderMode::Fill;
        let ndim           = mesh.normalize_dimensions();
        if *NUM_THREADS > 1 && (                  mesh.vtx.len() > 10000 ||
                               (do_vtx_shading && mesh.vtx.len() > 2500)) {
            // Parallel processing

            // Chunked iterators for both vertex input and output
            let vtx_chunk_size    = cmp::max(mesh.vtx.len() / *NUM_THREADS, 512);
            let vtx_transf_chunks = vtx_transf.chunks_mut(vtx_chunk_size);
            let vtx_mesh_chunks   = mesh.vtx.chunks(vtx_chunk_size);

            // Process chunks in parallel
            pool.scoped(|scoped| {
                for (cin, cout) in vtx_mesh_chunks.zip(vtx_transf_chunks) {
                    scoped.execute(move || {
                        transform_vertices(cin, cout, &ndim, w, h, &eye);
                        if do_vtx_shading {
                            for vtx in cout {
                                vtx.col = shader(&vtx.world, &vtx.n, &vtx.col, &eye, tick, cm);
                            }
                        }
                    });
                }

                // Do this before we start waiting for the pool
                bg_and_depth();
            });
        } else {
            // Serial processing

            transform_vertices
                (&mesh.vtx[..], &mut vtx_transf[..], &ndim, w, h, &eye);
            if do_vtx_shading {
                for vtx in &mut vtx_transf {
                    vtx.col = shader(&vtx.world, &vtx.n, &vtx.col, &eye, tick, cm);
                }
            }

            bg_and_depth();
        }
    }

    // Need to be able to send our buffer pointers across thread boundaries
    #[derive(Copy, Clone)]
    struct SendBufPtr<T> {
        ptr: *mut T
    }
    unsafe impl Send for SendBufPtr<u32> { }
    unsafe impl Send for SendBufPtr<f32> { }
    let send_fb    = SendBufPtr { ptr: fb };
    let send_depth = SendBufPtr { ptr: depth.as_mut_ptr() };

    // Draw
    match mode {
        RenderMode::Point => {
            for t in &mesh.tri {
                for idx in &[t.v0, t.v1, t.v2] {
                    let vp = &vtx_transf[*idx as usize].vp;
                    let x  = vp.x as i32;
                    let y  = vp.y as i32;

                    if x < 0 || x >= w || y < 0 || y >= h { continue }

                    let idx = x + y * w;
                    unsafe {
                        * fb.offset(idx as isize) = 0x00FFFFFF;
                    }
                }
            }
        }

        RenderMode::Line => {
            // Line drawing can be done in parallel
            pool.scoped(|scoped| {
                let vtx: &Vec<TransformedVertex> = &vtx_transf;
                for chunk in mesh.tri.chunks(cmp::max(mesh.tri.len() / *NUM_THREADS, 512)) {
                    scoped.execute(move || {
                        for t in chunk {
                            for &(idx1, idx2) in &[(t.v0, t.v1), (t.v1, t.v2), (t.v2, t.v0)] {
                                let vp1 = &vtx[idx1 as usize].vp;
                                let vp2 = &vtx[idx2 as usize].vp;
                                draw_line(vp1.x, vp1.y, vp2.x, vp2.y, send_fb.ptr, w, h);
                            }
                        }
                    });
                }
            });
        }

        RenderMode::Fill => {
            // Rasterize with a fixed-point half-space algorithm

            if *NUM_THREADS == 1 {
                // Serial implementation

                for t in &mesh.tri {
                    // Triangle vertices
                    let v0 = unsafe { vtx_transf.get_unchecked(t.v0 as usize) };
                    let v1 = unsafe { vtx_transf.get_unchecked(t.v1 as usize) };
                    let v2 = unsafe { vtx_transf.get_unchecked(t.v2 as usize) };

                    if shade_per_pixel {
                        rasterize_and_shade_triangle_pixel(
                            v0, v1, v2,
                            &shader, &eye, tick, cm,
                            0, 0, w, h,
                            w, send_fb.ptr, send_depth.ptr);
                    } else {
                        rasterize_and_shade_triangle_vertex(
                            v0, v1, v2,
                            &shader, &eye, tick, cm,
                            0, 0, w, h,
                            w, send_fb.ptr, send_depth.ptr);
                    }
                }
            } else {
                // Parallel implementation

                // Tiles
                static TILE_WDH: i32 = 64;
                static TILE_HGT: i32 = 64;
                static TILE_WDH_SHIFT: i32 = 6;
                static TILE_HGT_SHIFT: i32 = 6;
                struct Tile {
                    tri_idx: Vec<i32>,
                    x1:      i32,
                    y1:      i32,
                    x2:      i32,
                    y2:      i32
                };
                let tw = (w + TILE_WDH - 1) / TILE_WDH;
                let th = (h + TILE_HGT - 1) / TILE_HGT;

                // Build tile array
                let mut tiles = Vec::new();
                for ty in 0..th {
                    for tx in 0..tw {
                        let x1 = tx * TILE_WDH;
                        let y1 = ty * TILE_HGT;
                        let x2 = cmp::min(x1 + TILE_WDH, w); // Clip upper bound against FB
                        let y2 = cmp::min(y1 + TILE_HGT, h);

                        tiles.push(Tile {
                            // We can prevent a lot of allocations by starting
                            // with a reasonable capacity
                            tri_idx: Vec::with_capacity(256),
                            x1: x1,
                            y1: y1,
                            x2: x2,
                            y2: y2,
                        });
                    }
                }

                let vtx: &Vec<TransformedVertex> = &vtx_transf;

                // Bin mesh triangles into tiles
                //
                // TODO: This is slow for vertex heavy scenes, serial and duplicates
                //       work from the rasterizer like fixed-point conversion, AABB
                //       finding and backface culling. Optimize / unify this
                //
                for i in 0..mesh.tri.len() {
                    let t  = unsafe { &mesh.tri.get_unchecked(i as usize) };
                    let v0 = unsafe { vtx.get_unchecked(t.v0 as usize).vp };
                    let v1 = unsafe { vtx.get_unchecked(t.v1 as usize).vp };
                    let v2 = unsafe { vtx.get_unchecked(t.v2 as usize).vp };

                    let x0 = (v0.x * 16.0) as i32;
                    let y0 = (v0.y * 16.0) as i32;
                    let x1 = (v1.x * 16.0) as i32;
                    let y1 = (v1.y * 16.0) as i32;
                    let x2 = (v2.x * 16.0) as i32;
                    let y2 = (v2.y * 16.0) as i32;

                    let tri_a2 = (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0);
                    if tri_a2 <= 0 { continue }

                    let min_x = (min3(x0, x1, x2) + 0xF) >> 4;
                    let min_y = (min3(y0, y1, y2) + 0xF) >> 4;
                    let max_x = (max3(x0, x1, x2) + 0xF) >> 4;
                    let max_y = (max3(y0, y1, y2) + 0xF) >> 4;

                    // To tile coordinates
                    let min_x =  min_x >> TILE_WDH_SHIFT;
                    let min_y =  min_y >> TILE_HGT_SHIFT;
                    let max_x = (max_x >> TILE_WDH_SHIFT) + 1;
                    let max_y = (max_y >> TILE_HGT_SHIFT) + 1;

                    // Clip against framebuffer
                    let min_x = na::clamp(min_x, 0, tw);
                    let min_y = na::clamp(min_y, 0, th);
                    let max_x = na::clamp(max_x, 0, tw);
                    let max_y = na::clamp(max_y, 0, th);

                    // Add to tile bins
                    for ty in min_y..max_y {
                        for tx in min_x..max_x {
                            unsafe {
                                tiles.get_unchecked_mut
                                    ((tx + ty * tw) as usize).tri_idx.push(i as i32);
                            }
                        }
                    }
                }

                // Scheduling generally works better when we start with the most
                // expensive tiles, which tend to be the ones with the most triangles
                tiles.sort_by(|a, b| b.tri_idx.len().cmp(&a.tri_idx.len()));

                pool.scoped(|scoped| {
                    for tile in tiles {
                        // Don't create jobs for empty tiles
                        if tile.tri_idx.is_empty() { continue }

                        scoped.execute(move || {
                            for i in tile.tri_idx {
                                // Triangle vertices
                                let t  = unsafe { &mesh.tri.get_unchecked(i as usize) };
                                let v0 = unsafe { vtx.get_unchecked(t.v0 as usize) };
                                let v1 = unsafe { vtx.get_unchecked(t.v1 as usize) };
                                let v2 = unsafe { vtx.get_unchecked(t.v2 as usize) };

                                if shade_per_pixel {
                                    rasterize_and_shade_triangle_pixel(
                                        v0, v1, v2,
                                        &shader, &eye, tick, cm,
                                        tile.x1, tile.y1, tile.x2, tile.y2,
                                        w, send_fb.ptr, send_depth.ptr);
                                } else {
                                    rasterize_and_shade_triangle_vertex(
                                        v0, v1, v2,
                                        &shader, &eye, tick, cm,
                                        tile.x1, tile.y1, tile.x2, tile.y2,
                                        w, send_fb.ptr, send_depth.ptr);
                                }
                            }
                        });
                    }
                });
            }
        }
    }

    // Cube map unfolded LDR preview overlay
    if show_cm {
        cm.draw_cross(10, 10, w, h, fb);
    }
}

