
use na::{Vec2, Vec3, Vec4, Pnt3, Mat3, Mat4, Iso3, PerspMat3, Rot3};
use na::{Norm, Diag, Inv, Transpose, BaseFloat};
use na;
use std::path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;
use std::f32;

lazy_static! {
    static ref CORNELL_MESH: Mesh = {
        load_mesh(&String::from("data/cornell_radiosity.dat"), MeshFileType::XyzRgbXx)
    };
}

lazy_static! {
    static ref HEAD_MESH: Mesh = {
        load_mesh(&String::from("data/head_ao.dat"), MeshFileType:: XyzNxnynzAoao)
    };
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
        let extends_max =
            if extends.x > extends.y {
                if extends.x > extends.z { extends.x } else { extends.z }
            } else {
                if extends.y > extends.z { extends.y } else { extends.z }
            };
        let extends_scale = 1.0 / extends_max;
        let scale: Mat4<f32> =
            Diag::from_diag(&Vec4::new(extends_scale, extends_scale, extends_scale, 1.0));

        scale * na::to_homogeneous(&transf)
    }
}

fn face_normal(v0: &Pnt3<f32>, v1: &Pnt3<f32>,  v2: &Pnt3<f32>) -> Vec3<f32> {
    na::cross(&(*v1 - *v0), &(*v2 - *v0)).normalize()
}

// We either have 'Px Py Pz ColR ColB ColG UVx UVy' with bogus UVs and radiosity data
// in the color channel or 'Px Py Pz Nx Ny Nz UVx UVy' with ambient occlusion in both
// texture coordinates
#[derive(PartialEq)]
enum MeshFileType { XyzRgbXx, XyzNxnynzAoao }

fn load_mesh(file_name: &String, mesh_file_type: MeshFileType) -> Mesh {
    // Load a text format mesh from disk

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
                if words_collect.len() != 8 {
                    panic!("load_mesh(): Expected 8 component vertices: {}", display);
                }
                let mut components: Vec<f32> = Vec::new();
                for _ in 0..8 {
                    components.push(words.next().unwrap().parse::<f32>().unwrap());
                }
                match mesh_file_type {
                    MeshFileType::XyzRgbXx =>
                        vtx.push(Vertex::new(components[0],
                                             components[1],
                                             components[2],
                                             0.0, 0.0, 0.0, // Compute the normal later
                                             components[3],
                                             components[4],
                                             components[5]
                                            )),
                    MeshFileType::XyzNxnynzAoao =>
                        vtx.push(Vertex::new(components[0],
                                             components[1],
                                             components[2],
                                             components[3],
                                             components[4],
                                             components[5],
                                             components[6],
                                             components[6],
                                             components[6]
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
        if mesh_file_type == MeshFileType::XyzRgbXx {
            // Set vertex normals from face normal
            let n = face_normal(&vtx[tri_idx.0 as usize].p,
                                &vtx[tri_idx.1 as usize].p,
                                &vtx[tri_idx.2 as usize].p);
            vtx[tri_idx.0 as usize].n = n;
            vtx[tri_idx.1 as usize].n = n;
            vtx[tri_idx.2 as usize].n = n;
        }
        tri.push(ntri);
    }
    let mesh = Mesh::new(tri, vtx);

    // Print some mesh information
    println!("load_mesh(): Loaded {} Tri and {} Vtx from '{}', AABB ({}, {}, {}) - ({}, {}, {})",
        mesh.tri.len(), mesh.vtx.len(), display,
        mesh.aabb_min.x, mesh.aabb_min.y, mesh.aabb_min.z,
        mesh.aabb_max.x, mesh.aabb_max.y, mesh.aabb_max.z);

    mesh
}

fn look_at<N: BaseFloat>(eye: &Pnt3<N>, at: &Pnt3<N>, up: &Vec3<N>) -> Iso3<N> {
    // The Iso3::look_at_z() function form nalgebra seems broken, this works as expected

    let zaxis = na::normalize(&(*eye - *at));
    let xaxis = na::normalize(&na::cross(up, &zaxis));
    let yaxis = na::cross(&zaxis, &xaxis);

    let rot = unsafe {
        Rot3::new_with_mat(Mat3::new(
            xaxis.x, yaxis.x, zaxis.x,
            xaxis.y, yaxis.y, zaxis.y,
            xaxis.z, yaxis.z, zaxis.z)
        )
    };

    Iso3::new_with_rotmat(Vec3::new(na::dot(&xaxis, eye.as_vec()),
                                    na::dot(&yaxis, eye.as_vec()),
                                    na::dot(&zaxis, eye.as_vec())), rot)
}

fn deg_to_rad(deg: f32) -> f32 {
    // std::f32::to_radians() is still unstable
    deg * 0.0174532925
}

fn rgbf_to_abgr32(r: f32, g: f32, b: f32) -> u32 {
    let r8 = (na::clamp(r, 0.0, 1.0) * 255.0) as u32;
    let g8 = (na::clamp(g, 0.0, 1.0) * 255.0) as u32;
    let b8 = (na::clamp(b, 0.0, 1.0) * 255.0) as u32;
    r8 | (g8 << 8) | (b8 << 16)
}

#[repr(i32)]
pub enum RenderMode { Point, Line, Fill }

#[repr(i32)]
pub enum Scene { Head, CornellBox }

#[no_mangle]
pub extern fn rast_draw(mode: RenderMode,
                        scene: Scene,
                        bg_type: i32,
                        tick: f64,
                        w: i32,
                        h: i32,
                        fb: *mut u32) -> () {
    // Background gradient
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

    // Scene mesh
    let mesh: &Mesh = match scene {
        Scene::Head       => &HEAD_MESH,
        Scene::CornellBox => &CORNELL_MESH,
    };

    // Build mesh to screen transformation
    let world        = mesh.normalize_dimensions();
    let view         = na::to_homogeneous(
                           &look_at(
                               &match scene {
                                   Scene::Head => Pnt3::new(
                                       (tick.cos() * 2.0) as f32, 0.0, (tick.sin() * 2.0) as f32),
                                   Scene::CornellBox => Pnt3::new(
                                       (tick.cos() * 0.3) as f32, (tick.sin() * 0.3) as f32, 2.0),
                               },
                               &Pnt3::new(0.0, 0.0, 0.0),
                               &Vec3::y()));
    let proj         = *PerspMat3::new(
                           w as f32 / h as f32,
                           deg_to_rad(45.0),
                           0.01,
                           1000.0).as_mat();
    let wh           = w as f32 / 2.0;
    let hh           = h as f32 / 2.0;
    let screen       = Mat4::new(wh,  0.0, 0.0, wh,
                                 0.0, hh,  0.0, hh,
                                 0.0, 0.0, 1.0, 0.0,
                                 0.0, 0.0, 0.0, 1.0);
    let transf       = screen * proj * view * world;
    let transf_it_33 = na::from_homogeneous::<Mat4<f32>, Mat3<f32>>
                          (&transf.inv().unwrap().transpose());

    // Transform vertices. Transform and copy into uninitialized vector instead
    // of copy and transform in-place
    let mut vtx_transf: Vec<Vertex> = Vec::with_capacity(mesh.vtx.len());
    unsafe { vtx_transf.set_len(mesh.vtx.len()); }
    for i in 0..mesh.vtx.len() {
        let src = &mesh.vtx[i];
        let dst = &mut vtx_transf[i];
        // Homogeneous transform for the positions
        dst.p = na::from_homogeneous(&(transf * na::to_homogeneous(&src.p)));
        // Multiply with the 3x3 IT for normals
        dst.n = (transf_it_33 * src.n).normalize();
        // Copy color
        dst.col = src.col;
    }

    // Draw
    match mode {
        RenderMode::Point => {
            for t in &mesh.tri {

                // Backface culling. We can compute the signed triangle area with a cross
                // product, which in 2D boils down to the Z coordinate of the projected
                // triangle. If it's pointing away from the camera, we can discard it
                if false {
                    let v0 = &vtx_transf[t.v0 as usize].p;
                    let v1 = &vtx_transf[t.v1 as usize].p;
                    let v2 = &vtx_transf[t.v2 as usize].p;
                    let e1 = Vec2::new(v1.x - v0.x, v1.y - v0.y);
                    let e2 = Vec2::new(v2.x - v0.x, v2.y - v0.y);
                    if e1.x * e2.y - e1.y * e2.x <= 0.0 { continue }
                }

                for idx in &[t.v0, t.v1, t.v2] {
                    let proj = &vtx_transf[*idx as usize].p;
                    let x    = proj.x as i32;
                    let y    = proj.y as i32;

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
                    // Edge coordinates
                    let proj1  = &vtx_transf[idx1 as usize].p;
                    let mut x1 = proj1.x as i32;
                    let mut y1 = proj1.y as i32;
                    let proj2  = &vtx_transf[idx2 as usize].p;
                    let x2     = proj2.x as i32;
                    let y2     = proj2.y as i32;

                    let dx        = x2 - x1;
                    let dy        = y2 - y1;
                    let adx       = dx.abs();
                    let ady       = dy.abs();
                    let addx      = if dx < 0 { -1 } else { 1 };
                    let addy      = if dy < 0 { -1 } else { 1 };
                    let mut error = 0;
                    let mut i     = 0;

                    if adx > ady {
                        while {
                            error += ady;

                            // Time to move up / down ?
                            if error >= adx {
                                error -= adx;
                                y1 += addy;
                            }

                            i += 1;

                            // Move horizontally
                            x1 += addx;

                            // Bounds check and draw
                            if x1 >= 0 && x1 < w && y1 >= 0 && y1 < h {
                                let idx = x1 + y1 * w;
                                unsafe {
                                    * fb.offset(idx as isize) = 0x00FFFFFF;
                                }
                            }

                            // Repeat for x length of line
                            i < adx
                        } { }
                    } else {
                        while {
                            error += adx;

                            // Time to move left / right?
                            if error >= ady {
                                error -= ady;
                                x1 += addx;
                            }

                            i += 1;

                            // Move up / down a row
                            y1 += addy;

                            // Bounds check and draw
                            if x1 >= 0 && x1 < w && y1 >= 0 && y1 < h {
                                let idx = x1 + y1 * w;
                                unsafe {
                                    * fb.offset(idx as isize) = 0x00FFFFFF;
                                }
                            }

                            // Repeat for y length of line
                            i < ady
                        }
                        { }
                    }
                }
            }
        }

        RenderMode::Fill => {
        }
    }
}

