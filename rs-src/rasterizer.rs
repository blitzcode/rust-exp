
use na::{Vec3, Vec4, Pnt3, Pnt4, Mat3, Mat4, Iso3};
use na::{Norm, Diag, Inv, Transpose};
use na;
use std::path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;
use std::f32;
use std::f32::consts;
use stb_image::image;

lazy_static! {
  static ref CUBE_MESH: Mesh = load_mesh("meshes/cube.dat", MeshFileType::XyzNxNyNzRGB);
  static ref SPHERE_MESH: Mesh = load_mesh("meshes/sphere.dat", MeshFileType::XyzNxNyNz);
  static ref CORNELL_MESH: Mesh = load_mesh("meshes/cornell_radiosity.dat", MeshFileType::XyzRGB);
  static ref HEAD_MESH: Mesh = load_mesh("meshes/head_ao.dat", MeshFileType::XyzNxNyNzRGB);
  static ref TORUS_KNOT_MESH: Mesh = load_mesh("meshes/torus_knot.dat", MeshFileType::XyzNxNyNz);
  static ref KILLEROO_MESH: Mesh = load_mesh("meshes/killeroo_ao.dat", MeshFileType::XyzNxNyNzRGB);
  static ref HAND_MESH: Mesh = load_mesh("meshes/hand_ao.dat", MeshFileType::XyzNxNyNzRGB);
  static ref CAT_MESH: Mesh = load_mesh("meshes/cat_ao.dat", MeshFileType::XyzNxNyNzRGB);
}

#[derive(Clone, Copy)]
struct Vertex {
    p:   Pnt3<f32>,
    n:   Vec3<f32>,
    col: Vec3<f32>
}

impl Vertex {
    fn new(px: f32, py: f32, pz: f32,
           nx: f32, ny: f32, nz: f32,
           r:  f32, g:  f32, b:  f32) -> Vertex {
        Vertex {
            p:   Pnt3::new(px, py, pz),
            n:   Vec3::new(nx, ny, nz),
            col: Vec3::new(r , g , b)
        }
    }
}

// Indexed triangle representation
#[derive(Clone, Copy)]
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
    aabb_min: Vec3<f32>,
    aabb_max: Vec3<f32>
}

impl Mesh {
    fn new(tri: Vec<Triangle>, vtx: Vec<Vertex>) -> Mesh {
        // Compute AABB
        let mut mesh = Mesh { tri: tri, vtx: vtx, aabb_min: na::zero(), aabb_max: na::zero() };
        mesh.update_aabb();
        mesh
    }

    fn update_aabb(&mut self) {
        self.aabb_min = Vec3::new(f32::MAX, f32::MAX, f32::MAX);
        self.aabb_max = Vec3::new(f32::MIN, f32::MIN, f32::MIN);

        for v in &self.vtx {
            self.aabb_min.x = if self.aabb_min.x < v.p.x { self.aabb_min.x } else { v.p.x };
            self.aabb_min.y = if self.aabb_min.y < v.p.y { self.aabb_min.y } else { v.p.y };
            self.aabb_min.z = if self.aabb_min.z < v.p.z { self.aabb_min.z } else { v.p.z };
            self.aabb_max.x = if self.aabb_max.x > v.p.x { self.aabb_max.x } else { v.p.x };
            self.aabb_max.y = if self.aabb_max.y > v.p.y { self.aabb_max.y } else { v.p.y };
            self.aabb_max.z = if self.aabb_max.z > v.p.z { self.aabb_max.z } else { v.p.z };
        }
    }

    fn normalize_dimensions(&self) -> Mat4<f32> {
        // Build a matrix to transform the mesh to a unit cube with the origin as its center

        // Translate to center
        let center = (self.aabb_min + self.aabb_max) / 2.0;
        let transf = Iso3::new(-center, na::zero());

        // Scale to unit cube
        let extends = self.aabb_max - self.aabb_min;
        let extends_max = max3(extends.x, extends.y, extends.z);
        let extends_scale = 1.0 / extends_max;
        let scale: Mat4<f32> =
            Diag::from_diag(&Vec4::new(extends_scale, extends_scale, extends_scale, 1.0));

        scale * na::to_homogeneous(&transf)
    }
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

fn face_normal(v0: &Pnt3<f32>, v1: &Pnt3<f32>, v2: &Pnt3<f32>) -> Vec3<f32> {
    na::cross(&(*v1 - *v0), &(*v2 - *v0)).normalize()
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
            let v0p = Pnt3::new(vtx[tri_idx.0 as usize].p.x,
                                vtx[tri_idx.0 as usize].p.y,
                                vtx[tri_idx.0 as usize].p.z);
            let v1p = Pnt3::new(vtx[tri_idx.1 as usize].p.x,
                                vtx[tri_idx.1 as usize].p.y,
                                vtx[tri_idx.1 as usize].p.z);
            let v2p = Pnt3::new(vtx[tri_idx.2 as usize].p.x,
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
pub extern fn rast_get_mesh_tri_cnt(scene: Scene) -> i32 {
    mesh_from_enum(scene).tri.len() as i32
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

// Sets of pre-filtered irradiance cube maps
lazy_static! {
  static ref _CM_GRACE:       IrradianceCMSet = IrradianceCMSet::from_path("envmaps/grace"      );
  static ref _CM_PARKING_LOT: IrradianceCMSet = IrradianceCMSet::from_path("envmaps/parking_lot");
  static ref _CM_ENIS:        IrradianceCMSet = IrradianceCMSet::from_path("envmaps/enis"       );
  static ref _CM_GLACIER:     IrradianceCMSet = IrradianceCMSet::from_path("envmaps/glacier"    );
  static ref _CM_PISA:        IrradianceCMSet = IrradianceCMSet::from_path("envmaps/pisa"       );
  static ref _CM_PINE_TREE:   IrradianceCMSet = IrradianceCMSet::from_path("envmaps/pine_tree"  );
  static ref _CM_UFFIZI:      IrradianceCMSet = IrradianceCMSet::from_path("envmaps/uffizi"     );
  static ref _CM_DOGE:        IrradianceCMSet = IrradianceCMSet::from_path("envmaps/doge"       );
}

// All our irradiance cube map faces have the same fixed dimensions
static CM_FACE_WDH: i32 = 64;

#[derive(PartialEq, Debug, Copy, Clone)]
enum CMFaceName { XPos, XNeg, YPos, YNeg, ZPos, ZNeg }

type CMFace = Vec<Vec3<f32>>;
type CM     = [CMFace; 6];

struct IrradianceCMSet{
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
        for y in 0..self.cross_hgt {
            for x in 0..self.cross_wdh {
                let fbx = xorg + x;
                let fby = yorg + y;

                if fbx < 0 || fbx >= w || fby < 0 || fby >= h { continue }

                let fb_idx = (fbx + fby * w) as isize;
                let cr_idx = (x + y * self.cross_wdh) as usize;

                // Skip pixels not on the cross (alpha == 0)
                let c1 = self.cross[cr_idx];
                if c1 & 0xFF000000 == 0 { continue }

                // Blend
                unsafe {
                    let c  = fb.offset(fb_idx);
                    let c2 = *c;
                    *c     = avg_abgr32(c1, c2);
                }
            }
        }
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
                Vec3::new(img.data[(x * 3 + y * CM_FACE_WDH * 3 + 0) as usize],
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
    let cm = [
        load_cm_face(&cm_fn_from_param(path, power, CMFaceName::XPos), true , true ),
        load_cm_face(&cm_fn_from_param(path, power, CMFaceName::XNeg), false, true ),
        load_cm_face(&cm_fn_from_param(path, power, CMFaceName::YPos), false, false),
        load_cm_face(&cm_fn_from_param(path, power, CMFaceName::YNeg), false, true ),
        load_cm_face(&cm_fn_from_param(path, power, CMFaceName::ZPos), false, true ),
        load_cm_face(&cm_fn_from_param(path, power, CMFaceName::ZNeg), true , true )
    ];

    cm
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
                cross[idx] = rgbf_to_abgr32(col.x, col.y, col.z) | 0xFF000000;
            }
        }
    }

    (cross, cross_wdh, cross_hgt)
}

fn lookup_cm(cm: &CM, dir: &Vec3<f32>) -> Vec3<f32> {
    // Fetch the closest texel pointed at by 'dir' from passed cube map

    let face;
    let mut u;
    let mut v;
    let dir_abs = Vec3::new(dir.x.abs(), dir.y.abs(), dir.z.abs());

    if dir_abs.x > dir_abs.y && dir_abs.x > dir_abs.z  {
        if dir.x > 0.0  {
            face = CMFaceName::XPos;
        } else  {
            face = CMFaceName::XNeg;
        }
        u = dir.z / dir_abs.x;
        v = dir.y / dir_abs.x;
    } else if dir_abs.y > dir_abs.x && dir_abs.y > dir_abs.z  {
        if dir.y > 0.0  {
            face = CMFaceName::YPos;
        } else  {
            face = CMFaceName::YNeg;
        }
        u = dir.x / dir_abs.y;
        v = dir.z / dir_abs.y;
    } else  {
        if dir.z > 0.0  {
            face = CMFaceName::ZPos;
        } else  {
            face = CMFaceName::ZNeg;
        }
        u = dir.x / dir_abs.z;
        v = dir.y / dir_abs.z;
    }

    u = (u + 1.0) * 0.5;
    v = (v + 1.0) * 0.5;

    let tx = na::clamp((u * CM_FACE_WDH as f32) as i32, 0, CM_FACE_WDH - 1);
    let ty = na::clamp((v * CM_FACE_WDH as f32) as i32, 0, CM_FACE_WDH - 1);

    let idx = tx + ty * CM_FACE_WDH;

    cm[face as usize][idx as usize]
}

// The camera related functions in nalgebra like Iso3::look_at_z() and PerspMat3::new()
// are all using some rather unusual conventions and are not documented. Replace them with
// custom variants that work like the usual OpenGL style versions

fn look_at(eye: &Pnt3<f32>, at: &Pnt3<f32>, up: &Vec3<f32>) -> Mat4<f32> {
    let zaxis = na::normalize(&(*eye - *at));
    let xaxis = na::normalize(&na::cross(up, &zaxis));
    let yaxis = na::cross(&zaxis, &xaxis);

    Mat4::new(xaxis.x, xaxis.y, xaxis.z, na::dot(&-eye.to_vec(), &xaxis),
              yaxis.x, yaxis.y, yaxis.z, na::dot(&-eye.to_vec(), &yaxis),
              zaxis.x, zaxis.y, zaxis.z, na::dot(&-eye.to_vec(), &zaxis),
              0.0,     0.0,     0.0,     1.0)
}

fn perspective(fovy_deg: f32, aspect: f32, near: f32, far: f32) -> Mat4<f32> {
    let tan_half_fovy = (deg_to_rad(fovy_deg) / 2.0).tan();
    let m00 = 1.0 / (aspect * tan_half_fovy);
    let m11 = 1.0 / tan_half_fovy;
    let m22 = -(far + near) / (far - near);
    let m23 = -(2.0 * far * near) / (far - near);
    let m32 = -1.0;

    Mat4::new(m00, 0.0, 0.0, 0.0,
              0.0, m11, 0.0, 0.0,
              0.0, 0.0, m22, m23,
              0.0, 0.0, m32, 0.0)
}

fn deg_to_rad(deg: f32) -> f32 {
    // std::f32::to_radians() is still unstable
    deg * 0.0174532925
}

#[derive(Clone, Copy)]
struct TransformedVertex {
    vp:    Pnt4<f32>, // Projected, perspective divided, viewport transformed vertex with W
    world: Vec3<f32>, // World space vertex and normal for lighting computations etc.
    n:     Vec3<f32>, // ...
    col:   Vec3<f32>  // Color
}

fn transform_vertices(mesh: &Mesh, w: i32, h: i32, eye: &Pnt3<f32>) -> Vec<TransformedVertex> {
    // Build a mesh to viewport transformation and return a transformed set of vertices

    // Build transformations
    let mesh_to_world       = mesh.normalize_dimensions();
    let world_to_view       = look_at(eye, &Pnt3::new(0.0, 0.0, 0.0), &Vec3::y());
    let view_to_proj        = perspective(45.0, w as f32 / h as f32, 0.1, 10.0);
    let wh                  = w as f32 / 2.0;
    let hh                  = h as f32 / 2.0;
                              // TODO: We're applying the viewport transform before the
                              //       perspective divide, why does this actually work
                              //       identically to doing it right after it below?
    let proj_to_vp          = Mat4::new(wh,  0.0, 0.0, wh,
                                        0.0, hh,  0.0, hh,
                                        0.0, 0.0, 1.0, 0.0,
                                        0.0, 0.0, 0.0, 1.0);
    let world_to_vp         = proj_to_vp * view_to_proj * world_to_view;
    let mesh_to_world_it_33 = na::from_homogeneous::<Mat4<f32>, Mat3<f32>>
                                  (&mesh_to_world.inv().unwrap().transpose());

    // Transform and copy into uninitialized vector instead of copy and transform in-place
    let mut vtx_transf: Vec<TransformedVertex> = Vec::with_capacity(mesh.vtx.len());
    unsafe { vtx_transf.set_len(mesh.vtx.len()); }
    for i in 0..mesh.vtx.len() {
        let src = &mesh.vtx[i];
        let dst = &mut vtx_transf[i];

        // Transform from mesh into world space
        let world_h: Pnt4<f32> = mesh_to_world * na::to_homogeneous(&src.p);
        dst.world = Vec3::new(world_h.x, world_h.y, world_h.z);

        // World to viewport. Note that we do the perspective divide manually instead of using
        //   dst = na::from_homogeneous(&(transf * na::to_homogeneous(&src)));
        // so we can keep W around
        dst.vp    = world_to_vp * world_h;
        let inv_w = 1.0 / dst.vp.w;
        dst.vp.x *= inv_w;
        dst.vp.y *= inv_w;
        dst.vp.z *= inv_w;

        // Multiply with the 3x3 IT for world space normals
        dst.n = mesh_to_world_it_33 * src.n;

        // Copy color
        dst.col = src.col;
    }

    vtx_transf
}

fn draw_bg_gradient(bg_type: i32, w: i32, h: i32, fb: *mut u32) {
    // Fill the framebuffer with a vertical gradient

    let start;
    let end;
    match bg_type % 5 {
        0 => { start = Vec3::new(0.3, 0.3, 0.3); end = Vec3::new(0.7, 0.7, 0.7); }
        1 => { start = Vec3::new(1.0, 0.4, 0.0); end = Vec3::new(0.0, 0.5, 0.5); }
        2 => { start = Vec3::new(1.0, 0.0, 1.0); end = Vec3::new(1.0, 0.0, 1.0); }
        3 => { start = Vec3::new(1.0, 1.0, 1.0); end = Vec3::new(1.0, 1.0, 1.0); }
        _ => { start = Vec3::new(0.0, 0.0, 0.0); end = Vec3::new(0.0, 0.0, 0.0); }
    }

    for y in 0..h {
        let pos   = y as f32 / (h - 1) as f32;
        let col   = start * (1.0 - pos) + end * pos;
        let col32 = rgbf_to_abgr32(col.x, col.y, col.z);

        for x in 0..w {
            unsafe {
                * fb.offset((x + y * w) as isize) = col32;
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

fn reflect(i: &Vec3<f32>, n: &Vec3<f32>) -> Vec3<f32> {
    // GLSL style reflection vector function
    *i - (*n * na::dot(n, i) * 2.0)
}

// All shaders have this signature
type Shader = fn(&Vec3<f32>, // World space position
                 &Vec3<f32>, // World space normal
                 &Vec3<f32>, // Color
                 &Pnt3<f32>, // World space camera position
                 f64) ->     // Current time (tick)
                 Vec3<f32>;  // Output color

fn shader_color(_p: &Vec3<f32>,
                _n: &Vec3<f32>,
                col: &Vec3<f32>,
                _eye: &Pnt3<f32>,
                _tick: f64) -> Vec3<f32> {
    // Just use the static mesh color
    *col
}

fn shader_n_to_color(_p: &Vec3<f32>,
                     n: &Vec3<f32>,
                     _col: &Vec3<f32>,
                     _eye: &Pnt3<f32>,
                     _tick: f64) -> Vec3<f32> {
    // Convert the normal to a color
    (n.normalize() + 1.0) * 0.5
}

fn shader_dir_light_ao(p: &Vec3<f32>,
                       n: &Vec3<f32>,
                       col: &Vec3<f32>,
                       eye: &Pnt3<f32>,
                       _tick: f64) -> Vec3<f32> {
    // Specular material lit by two light sources, mesh color is treated as AO factor

    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vec();
    let r   = fast_normalize(&reflect(&eye, &n));
    let l   = Vec3::new(0.577350269, 0.577350269, 0.577350269); // Normalized (1, 1, 1)

    let light_1;
    {
        let ldotn =                 na::clamp(na::dot(&l, &n), 0.0, 1.0);
        let ldotr = fast_unit_pow16(na::clamp(na::dot(&l, &r), 0.0, 1.0));
        light_1   = ldotn * 0.75 + ldotr;
    }

    let light_2;
    {
        let ldotn =                 na::clamp(na::dot(&-l, &n), 0.0, 1.0);
        let ldotr = fast_unit_pow16(na::clamp(na::dot(&-l, &r), 0.0, 1.0));
        light_2   = ldotn * 0.75 + ldotr;
    }

    let ambient   = Vec3::new(0.1, 0.1, 0.1);
    let light     = Vec3::new(1.0, 0.75, 0.75) * light_1 +
                    Vec3::new(0.75, 1.0, 1.0)  * light_2 +
                    ambient;
    let occlusion = *col * *col;

    light * occlusion
}

fn normalize_phong_lobe(power: f32) -> f32
{
    (power + 2.0) / 2.0
}

fn shader_cm_refl(p: &Vec3<f32>,
                 n: &Vec3<f32>,
                 col: &Vec3<f32>,
                 eye: &Pnt3<f32>,
                 _tick: f64) -> Vec3<f32> {
    let n   = fast_normalize(n);
    let eye = *p - *eye.as_vec();
    let r   = fast_normalize(&reflect(&eye, &n));

    // lookup_cm(&_CM_GRACE.cos_1, &n) * 5.0 * *col
    (
      lookup_cm(&_CM_GRACE.cos_1,  &n) * 6.0
    + lookup_cm(&_CM_GRACE.cos_8,  &r) * normalize_phong_lobe(8.0 ) * 2.0
    //+ lookup_cm(&_CM_GRACE.cos_512, &r) * normalize_phong_lobe(512.0) * 2.2
    )
    * (*col * *col)
}
fn fast_normalize(n: &Vec3<f32>) -> Vec3<f32> {
    // nalgbera doesn't use a reciprocal
    let l = 1.0 / (n.x * n.x + n.y * n.y + n.z * n.z).sqrt();
    Vec3::new(n.x * l, n.y * l, n.z * l)
}

fn fast_unit_pow16(v: f32) -> f32 {
    // Fast X^16 function for X e [0, 1] using a 256 entry lookup table
    //
    // Table generation:
    //
    // for i in 600..256 + 600 {
    //     let v = i as f32 / (600.0 + 255.0);
    //     println!("    {},", v.powf(16.0));
    // }

    static TBL: [f32; 256] = [
        0.003459093, 0.003552495, 0.0036482627, 0.0037464416, 0.0038470984, 0.003950281,
        0.0040560593, 0.004164483, 0.004275625, 0.004389537, 0.004506296, 0.0046259556,
        0.004748595, 0.0048742713, 0.005003067, 0.005135042, 0.005270282, 0.005408848,
        0.0055508246, 0.0056962967, 0.0058453293, 0.005998019, 0.006154434, 0.006314675,
        0.0064788125, 0.006646952, 0.006819167, 0.0069955676, 0.0071762297, 0.0073612686,
        0.0075507634, 0.0077448343, 0.007943563, 0.008147076, 0.008355458, 0.00856884,
        0.008787312, 0.009010997, 0.009240023, 0.009474486, 0.00971453, 0.009960255,
        0.01021181, 0.010469298, 0.010732877, 0.011002653, 0.011278792, 0.011561403,
        0.011850657, 0.012146669, 0.012449619, 0.012759625, 0.0130768735, 0.013401488,
        0.013733663, 0.0140735265, 0.014421281, 0.0147770615, 0.015141058, 0.015513466,
        0.015894428, 0.016284168, 0.016682833, 0.017090656, 0.017507788, 0.017934473,
        0.018370869, 0.018817227, 0.019273717, 0.019740595, 0.020218033, 0.020706309,
        0.021205597, 0.02171618, 0.022238243, 0.022772085, 0.023317892, 0.023875948,
        0.024446534, 0.025029855, 0.025626237, 0.026235888, 0.026859147, 0.027496237,
        0.028147504, 0.028813178, 0.029493624, 0.030189078, 0.030899918, 0.03162639,
        0.03236889, 0.03312767, 0.033903137, 0.03469556, 0.03550536, 0.036332812,
        0.03717832, 0.03804229, 0.038925007, 0.03982695, 0.040748402, 0.04168987,
        0.042651646, 0.043634243, 0.04463798, 0.04566339, 0.046710797, 0.047780745,
        0.048873585, 0.04998988, 0.051129986, 0.052294493, 0.05348377, 0.05469843,
        0.05593885, 0.05720567, 0.05849928, 0.059820276, 0.06116927, 0.06254667,
        0.06395318, 0.06538923, 0.06685554, 0.06835256, 0.06988104, 0.07144143,
        0.073034525, 0.0746608, 0.07632106, 0.0780158, 0.07974585, 0.081511736,
        0.08331432, 0.08515413, 0.08703208, 0.0889487, 0.09090483, 0.09290134,
        0.0949388, 0.097018205, 0.09914013, 0.10130563, 0.10351528, 0.105770186,
        0.108070955, 0.1104187, 0.112814076, 0.115258224, 0.11775182, 0.12029606,
        0.122891635, 0.12553978, 0.1282412, 0.1309972, 0.1338085, 0.13667643,
        0.13960177, 0.14258571, 0.14562954, 0.14873405, 0.15190066, 0.15513025,
        0.15842429, 0.16178365, 0.16520987, 0.16870385, 0.1722672, 0.17590083,
        0.17960641, 0.18338488, 0.18723798, 0.19116667, 0.19517274, 0.19925721,
        0.20342192, 0.20766792, 0.21199688, 0.21641058, 0.22091007, 0.2254974,
        0.23017366, 0.23494098, 0.23980048, 0.24475436, 0.2498038, 0.25495103,
        0.2601973, 0.26554492, 0.27099517, 0.27655044, 0.28221205, 0.2879825,
        0.2938631, 0.29985642, 0.3059639, 0.31218818, 0.31853068, 0.32499382,
        0.3315801, 0.33829102, 0.34512943, 0.35209695, 0.3591965, 0.36642978,
        0.37379977, 0.3813082, 0.38895822, 0.39675155, 0.4046915, 0.4127798,
        0.4210199, 0.4294136, 0.4379644, 0.4466742, 0.45554665, 0.46458364,
        0.47378847, 0.48316458, 0.49271393, 0.5024405, 0.5123463, 0.52243555,
        0.5327103, 0.5431748, 0.5538312, 0.564684, 0.5757353, 0.58698964,
        0.59844947, 0.61011934, 0.6220017, 0.6341014, 0.64642084, 0.65896505,
        0.67173654, 0.6847404, 0.6979794, 0.711458, 0.7251811, 0.73915136,
        0.7533743, 0.7678528, 0.78259254, 0.7975965, 0.81287056, 0.8284178,
        0.8442442, 0.86035293, 0.87675035, 0.8934395, 0.91042703, 0.9277161,
        0.9453135, 0.96322256, 0.98145026, 1.0
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

fn mesh_from_enum<'a>(scene: Scene) -> &'a Mesh {
    match scene {
        Scene::Cube       => &CUBE_MESH,
        Scene::Sphere     => &SPHERE_MESH,
        Scene::CornellBox => &CORNELL_MESH,
        Scene::Head       => &HEAD_MESH,
        Scene::TorusKnot  => &TORUS_KNOT_MESH,
        Scene::Killeroo   => &KILLEROO_MESH,
        Scene::Hand       => &HAND_MESH,
        Scene::Cat        => &CAT_MESH
    }
}

fn smootherstep(edge0: f32, edge1: f32, x: f32) -> f32
{
    // Scale and clamp x to 0..1 range
    let x = na::clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    // Evaluate polynomial
    x * x * x * (x * (x * 6.0 - 15.0) + 10.0)
}

fn build_scene<'a>(scene: Scene, tick: f64) -> (&'a Mesh, Shader, Pnt3<f32>) {
    // Build a scene (mesh, shader, camera position)

    let mesh = mesh_from_enum(scene);

    let shader: Shader = match scene {
        Scene::Cube       => shader_color,
        Scene::Sphere     => shader_n_to_color,
        Scene::CornellBox => shader_color,
        Scene::Head       => shader_dir_light_ao,
        Scene::TorusKnot  => shader_cm_refl,
        Scene::Killeroo   => shader_cm_refl,
        Scene::Hand       => shader_color,
        Scene::Cat        => shader_dir_light_ao
    };

    let eye = match scene {
        Scene::Cube   |
        Scene::Sphere |
        Scene::TorusKnot =>
            // Orbit around object
            Pnt3::new(((tick / 1.25).cos() * 1.8) as f32,
                      0.0,
                      ((tick / 1.25).sin() * 1.8) as f32),

        Scene::Head |
        Scene::Hand |
        Scene::Cat =>
            // Orbit closer around object
            Pnt3::new(((tick / 1.25).cos() * 1.6) as f32,
                      0.0,
                      ((tick / 1.25).sin() * 1.6) as f32),

        Scene::Killeroo => {
            // Slow, dampened pan around the front of the object,
            // some slow vertical bobbing as well
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
            Pnt3::new(tick_seg.cos() as f32,
                      ((tick / 2.0).sin() * 0.25 + 0.2) as f32,
                      tick_seg.sin() as f32)
        }

        Scene::CornellBox =>
            // Camera makes circular motion looking at the box (which is open at the back)
            Pnt3::new((tick.cos() * 0.3) as f32,
                      (tick.sin() * 0.3) as f32,
                      -2.0)
    };

    (mesh, shader, eye)
}

fn rgbf_to_abgr32(r: f32, g: f32, b: f32) -> u32 {
    let r8 = (na::clamp(r, 0.0, 1.0) * 255.0) as u32;
    let g8 = (na::clamp(g, 0.0, 1.0) * 255.0) as u32;
    let b8 = (na::clamp(b, 0.0, 1.0) * 255.0) as u32;
    r8 | (g8 << 8) | (b8 << 16)
}

fn avg_abgr32(c1: u32, c2: u32) -> u32 {
    // Clever averaging of bytes in two words, see http://tinyurl.com/jzldn9e
    (((c1 ^ c2) >> 1) & 0x7F7F7F7F) + (c1 & c2)
}

#[repr(i32)]
#[derive(PartialEq)]
pub enum RenderMode { Point, Line, Fill }

#[repr(i32)]
#[derive(Copy, Clone)]
pub enum Scene { Cube, Sphere, CornellBox, Head, TorusKnot, Killeroo, Hand, Cat  }

#[no_mangle]
pub extern fn rast_draw(shade_per_pixel: i32,
                        mode: RenderMode,
                        scene: Scene,
                        bg_type: i32,
                        tick: f64,
                        w: i32,
                        h: i32,
                        fb: *mut u32) -> () {
    // Transform, rasterize and shade mesh

    // Avoid passing a bool over the FFI, convert now
    let shade_per_pixel: bool = shade_per_pixel != 0;

    // let tick: f64 = 0.0;

    // Scene
    let (mesh, shader, eye) = build_scene(scene, tick);

    // Transform
    let mut vtx_transf = transform_vertices(&mesh, w, h, &eye);

    // Do vertex shading?
    if !shade_per_pixel && mode == RenderMode::Fill {
        for vtx in &mut vtx_transf {
            vtx.col = shader(&vtx.world, &vtx.n, &vtx.col, &eye, tick);
        }
    }

    // Background gradient
    draw_bg_gradient(bg_type, w, h, fb);

    // Draw
    match mode {
        RenderMode::Point => {
            for t in &mesh.tri {
                for idx in &[t.v0, t.v1, t.v2] {
                    let vp = &vtx_transf[*idx as usize].vp;
                    let x    = vp.x as i32;
                    let y    = vp.y as i32;

                    if x < 0 || x >= w || y < 0 || y >= h { continue }

                    let idx = x + y * w;
                    unsafe {
                        * fb.offset(idx as isize) = 0x00FFFFFF;
                    }
                }
            }
        }

        RenderMode::Line => {
            for t in &mesh.tri {
                for &(idx1, idx2) in &[(t.v0, t.v1), (t.v1, t.v2), (t.v2, t.v0)] {
                    let vp1 = &vtx_transf[idx1 as usize].vp;
                    let vp2 = &vtx_transf[idx2 as usize].vp;

                    draw_line(vp1.x, vp1.y, vp2.x, vp2.y, fb, w, h);
                }
            }
        }

        RenderMode::Fill => {
            // Rasterize with a fixed-point half-space algorithm

            // Allocate and clear depth buffer
            let mut depth: Vec<f32> = Vec::new();
            depth.resize((w * h) as usize, 1.0);
            let depth_ptr = depth.as_mut_ptr();

            for t in &mesh.tri {
                // Triangle vertices
                let vtx0 = &vtx_transf[t.v0 as usize];
                let vtx1 = &vtx_transf[t.v1 as usize];
                let vtx2 = &vtx_transf[t.v2 as usize];

                // Break out positions (viewport and world), colors and normals
                let v0 = &vtx0.vp; let p0 = &vtx0.world; let c0 = &vtx0.col; let n0 = &vtx0.n;
                let v1 = &vtx1.vp; let p1 = &vtx1.world; let c1 = &vtx1.col; let n1 = &vtx1.n;
                let v2 = &vtx2.vp; let p2 = &vtx2.world; let c2 = &vtx2.col; let n2 = &vtx2.n;

                // Convert to 28.4 fixed-point
                let x0 = (v0.x * 16.0).round() as i32;
                let y0 = (v0.y * 16.0).round() as i32;
                let x1 = (v1.x * 16.0).round() as i32;
                let y1 = (v1.y * 16.0).round() as i32;
                let x2 = (v2.x * 16.0).round() as i32;
                let y2 = (v2.y * 16.0).round() as i32;

                // Backface culling through cross product. The Z component of the
                // resulting vector tells us if the triangle is facing the camera or not,
                // its magnitude is the 2x the signed area of the triangle, which is
                // exactly what we need to normalize our barycentric coordinates later
                let tri_a2 = (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0);
                if tri_a2 <= 0 { continue }
                let inv_tri_a2 = 1.0 / tri_a2 as f32;

                // We test triangle coverage at integer coordinates D3D9 style vs centers
                // like >=D3D10 and OpenGL. Our pixel center is at the bottom-left. We
                // also use a bottom-left fill convention, unlike the more conventional
                // top-left one. It's what D3D does, but with the Y axis flipped (our
                // origin is bottom-left). We normally consider only raster positions as
                // on the inside of an edge if the half-space function returns a positive
                // value. For the bottom-left edges we want the contested raster positions
                // lying on the edge to belong to its triangle.
                //
                // Consider these two 2x2 triangulated quads and how they'd rasterize with
                // each convention
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
                // With a bottom-left coordinate system origin and pixel center the latter
                // fill convention just makes more sense to me. No need to shift our AABB's
                // Y by one and we can just round up on both min / max bound

                // AABB of the triangle, map to pixels by rounding up
                let min_x = (min3(x0, x1, x2) + 0xF) >> 4;
                let min_y = (min3(y0, y1, y2) + 0xF) >> 4;
                let max_x = (max3(x0, x1, x2) + 0xF) >> 4;
                let max_y = (max3(y0, y1, y2) + 0xF) >> 4;

                // Clip against framebuffer
                let min_x = na::clamp(min_x, 0, w);
                let min_y = na::clamp(min_y, 0, h);
                let max_x = na::clamp(max_x, 0, w);
                let max_y = na::clamp(max_y, 0, h);

                // Implement bottom-left fill convention. Classifying those edges is
                // simple, as with CCW vertex order they are either descending or
                // horizontally moving left to right. We basically want to turn the '> 0'
                // comparison into '>= 0', and we do this by adding a constant 1 for the
                // half-space functions of those edges
                let e0add = if (y1 - y0) < 0 || ((y1 - y0) == 0 && (x1 - x0) > 0) { 1 } else { 0 };
                let e1add = if (y2 - y1) < 0 || ((y2 - y1) == 0 && (x2 - x1) > 0) { 1 } else { 0 };
                let e2add = if (y0 - y2) < 0 || ((y0 - y2) == 0 && (x0 - x2) > 0) { 1 } else { 0 };

                for y in min_y..max_y {
                    for x in min_x..max_x {
                        // 28.4 coordinates of the current raster position
                        let xf = x << 4;
                        let yf = y << 4;

                        // Check the half-space functions for all three edges to see if
                        // we're inside the triangle. These functions are basically just a
                        // cross product between an edge and a vector from the current
                        // raster position to the edge. The resulting vector will either
                        // point into or out of the screen, so we can check which side of
                        // the edge we're on by the sign of the Z component
                        let hs0 = (x1 - x0) * (yf - y0) - (y1 - y0) * (xf - x0) + e0add;
                        let hs1 = (x2 - x1) * (yf - y1) - (y2 - y1) * (xf - x1) + e1add;
                        let hs2 = (x0 - x2) * (yf - y2) - (y0 - y2) * (xf - x2) + e2add;
                        if hs0 > 0 && hs1 > 0 && hs2 > 0 {
                            // The cross product from the edge function not only tells us
                            // which side we're on, but also the area of parallelogram
                            // formed by the two vectors. We're basically getting twice
                            // the area of one of the three triangles splitting the
                            // original triangle around the raster position. Those are
                            // already barycentric coordinates, we just need to normalize
                            // them with the triangle area we already computed with the
                            // cross product for the backface culling test. Don't forget
                            // to remove the fill convention bias applied earlier
                            let b0 = (hs0 - e0add) as f32 * inv_tri_a2;
                            let b1 = (hs1 - e1add) as f32 * inv_tri_a2;
                            let b2 = (hs2 - e2add) as f32 * inv_tri_a2;

                            let idx = (x + y * w) as isize;

                            // Interpolate, test and write depth. Note that we are
                            // interpolating z/w, which is linear in screen space, no
                            // special perspective correct interpolation required. We
                            // also use a Z buffer, not a W buffer
                            let z = v0.z * b1 + v1.z * b2 + v2.z * b0;
                            unsafe {
                                let d = depth_ptr.offset(idx);
                                if z > *d { continue }
                                *d = z;
                            }

                            let inv_w_0 = 1.0 / vtx0.vp.w;
                            let inv_w_1 = 1.0 / vtx1.vp.w;
                            let inv_w_2 = 1.0 / vtx2.vp.w;

                            // To do perspective correct interpolation of attributes we
                            // need to know w at the current raster position. We can
                            // compute it by interpolating 1/w linearly and then taking
                            // the reciprocal
                            let w_raster = 1.0 / (inv_w_0 * b1 + inv_w_1 * b2 + inv_w_2 * b0);

                            // Interpolate color. Perspective correct interpolation requires
                            // us to linearly interpolate col/w and then multiply by w
                            let c_raster = (* c0 * inv_w_0 * b1 +
                                            * c1 * inv_w_1 * b2 +
                                            * c2 * inv_w_2 * b0) * w_raster;

                            // Shading
                            let out = if shade_per_pixel {
                                // Also do perspective correct interpolation of the vertex normal
                                // and world space position, the shader might want these
                                let p_raster = (* p0 * inv_w_0 * b1 +
                                                * p1 * inv_w_1 * b2 +
                                                * p2 * inv_w_2 * b0) * w_raster;
                                let n_raster = (* n0 * inv_w_0 * b1 +
                                                * n1 * inv_w_1 * b2 +
                                                * n2 * inv_w_2 * b0) * w_raster;

                                // Call shader
                                shader(&p_raster, &n_raster, &c_raster, &eye, tick)
                            } else {
                                // Just write interpolated per-vertex shading result
                                c_raster
                            };

                            // Write color
                            unsafe {
                                * fb.offset(idx) = rgbf_to_abgr32(out.x, out.y, out.z);
                            }
                        }
                    }
                }
            }
        }
    }

    // Cube map unfolded LDR preview overlay
    _CM_GRACE.draw_cross(10, 10, w, h, fb);
}

