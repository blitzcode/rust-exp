
use na::{Vec3, Vec4, Pnt3, Pnt4, Mat3, Mat4, Iso3, PerspMat3, Rot3};
use na::{Norm, Diag, Inv, Transpose, BaseFloat};
use na;
use std::path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;
use std::f32;

lazy_static! {
    static ref CUBE_MESH: Mesh = {
        load_mesh(&String::from("data/cube.dat"), MeshFileType::XyzNxNyNzRGB)
    };
    static ref SPHERE_MESH: Mesh = {
        load_mesh(&String::from("data/sphere.dat"), MeshFileType::XyzNxNyNz)
    };
    static ref CORNELL_MESH: Mesh = {
        load_mesh(&String::from("data/cornell_radiosity.dat"), MeshFileType::XyzRGB)
    };
    static ref HEAD_MESH: Mesh = {
        load_mesh(&String::from("data/head_ao.dat"), MeshFileType::XyzNxNyNzRGB)
    };
    static ref TORUS_KNOT_MESH: Mesh = {
        load_mesh(&String::from("data/torus_knot.dat"), MeshFileType::XyzNxNyNz)
    };
}

#[derive(Clone, Copy)]
struct Vertex {
    p:   Pnt4<f32>, // Four components, want to be able to store W post-projection
    n:   Vec3<f32>,
    col: Vec3<f32>
}

impl Vertex {
    fn new(px: f32, py: f32, pz: f32,
           nx: f32, ny: f32, nz: f32,
           r:  f32, g:  f32, b:  f32) -> Vertex {
        Vertex {
            p:   Pnt4::new(px, py, pz, 1.0),
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
                                             // Just derive colors from normal
                                             (components[3] + 1.0) / 2.0,
                                             (components[4] + 1.0) / 2.0,
                                             (components[5] + 1.0) / 2.0
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

fn draw_line(x1: f32, y1: f32, x2: f32, y2: f32, fb: *mut u32, w: i32, h: i32) {
    // Draw a line using the DDA algorithm. This is pretty poor code for various
    // reasons, but just about good enough for debug drawing some wire frames

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

#[repr(i32)]
pub enum RenderMode { Point, Line, Fill }

#[repr(i32)]
pub enum Scene { Cube, Sphere, CornellBox, Head, TorusKnot  }

#[no_mangle]
pub extern fn rast_draw(mode: RenderMode,
                        scene: Scene,
                        bg_type: i32,
                        tick: f64,
                        w: i32,
                        h: i32,
                        fb: *mut u32) -> () {
    // Transform, rasterize and shade mesh

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
        Scene::Sphere     => &SPHERE_MESH,
        Scene::Cube       => &CUBE_MESH,
        Scene::CornellBox => &CORNELL_MESH,
        Scene::Head       => &HEAD_MESH,
        Scene::TorusKnot  => &TORUS_KNOT_MESH
    };

    // Build mesh to screen transformation
    let world        = mesh.normalize_dimensions();
    let view         = na::to_homogeneous(
                           &look_at(
                               &match scene {
                                   Scene::Cube   |
                                   Scene::Sphere |
                                   Scene::Head   |
                                   Scene::TorusKnot  => Pnt3::new(
                                       (tick.cos() * 2.0) as f32, 0.0, (tick.sin() * 2.0) as f32),
                                   Scene::CornellBox => Pnt3::new(
                                       (tick.cos() * 0.3) as f32, (tick.sin() * 0.3) as f32, 2.0),
                               },
                               &Pnt3::new(0.0, 0.0, 0.0),
                               &Vec3::y()));
    let proj         = *PerspMat3::new(
                           w as f32 / h as f32,
                           deg_to_rad(45.0),
                           0.1,
                           10.0).as_mat();
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
        // Homogeneous transform for the positions. Note that we do the perspective divide
        // manually instead of using
        //
        // dst.p = na::from_homogeneous(&(transf * na::to_homogeneous(&src.p)));
        //
        // so we can keep W around
        dst.p = transf * src.p;
        let inv_w = 1.0 / dst.p.w;
        dst.p.x *= inv_w;
        dst.p.y *= inv_w;
        dst.p.z *= inv_w;
        // Multiply with the 3x3 IT for normals
        dst.n = (transf_it_33 * src.n).normalize();
        // Copy color
        dst.col = src.col;
    }

    // Draw
    match mode {
        RenderMode::Point => {
            for t in &mesh.tri {
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
                    let proj1 = &vtx_transf[idx1 as usize].p;
                    let proj2 = &vtx_transf[idx2 as usize].p;

                    draw_line(proj1.x, proj1.y, proj2.x, proj2.y, fb, w, h);
                }
            }
        }

        RenderMode::Fill => {
            // Rasterize with a fixed-point half-space algorithm

            // Allocate depth buffer
            let mut depth: Vec<f32> = Vec::new();
            depth.resize((w * h) as usize, 1.0);
            let depth_ptr = depth.as_mut_ptr();

            for t in &mesh.tri {
                // Triangle vertices
                let vtx0 = &vtx_transf[t.v0 as usize];
                let vtx1 = &vtx_transf[t.v1 as usize];
                let vtx2 = &vtx_transf[t.v2 as usize];

                // Break out positions, colors and normals
                let v0 = &vtx0.p; let c0 = &vtx0.col; let n0 = &vtx0.n;
                let v1 = &vtx1.p; let c1 = &vtx1.col; let n1 = &vtx1.n;
                let v2 = &vtx2.p; let c2 = &vtx2.col; let n2 = &vtx2.n;

                // Convert to 28.4 fixed-point
                let x0 = (v0.x * 16.0).round() as i32;
                let y0 = (v0.y * 16.0).round() as i32;
                let x1 = (v1.x * 16.0).round() as i32;
                let y1 = (v1.y * 16.0).round() as i32;
                let x2 = (v2.x * 16.0).round() as i32;
                let y2 = (v2.y * 16.0).round() as i32;

                // Edges

                // Backface culling through cross product. The Z component of the
                // resulting vector tells us if the triangle is facing the camera or not,
                // its magnitude is the 2x the signed area of the triangle, which is
                // exactly what we need to normalize our barycentric coordinates later
                let tri_a2 = (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0);
                if tri_a2 <= 0 { continue }
                let inv_tri_a2 = 1.0 / tri_a2 as f32;

                // Implement top-left fill convention. Classifying those edges is simple,
                // as with CCW vertex order they are either descending or horizontally
                // moving right to left. We normally consider only raster positions as on
                // the inside of an edge if the half-space function returns a positive
                // value. For the top-left edges we want the contested raster positions
                // lying on the edge to belong to its triangle. We basically want to turn
                // the '> 0' comparison into '>= 0', and we do this by adding a constant 1
                // for the half-space functions of those edges
                let e0add = if (y1 - y0) < 0 || ((y1 - y0) == 0 && (x1 - x0) > 0) { 1 } else { 0 };
                let e1add = if (y2 - y1) < 0 || ((y2 - y1) == 0 && (x2 - x1) > 0) { 1 } else { 0 };
                let e2add = if (y0 - y2) < 0 || ((y0 - y2) == 0 && (x0 - x2) > 0) { 1 } else { 0 };

                // AABB of the triangle. We can safely round up on the lower bound as
                // there is no chance for coverage of the lower-left corner which we take
                // as the pixel center. Round up on the upper
                // bound as the loop over the AABB interprets the bounds as an open
                // interval and could otherwise miss intersections. Note that we only need
                // to round up and not do a +1 since our fill convention assigns those
                // intersections on the
                let min_x = (min3(x0, x1, x2) + 0xF) >> 4;
                let min_y = (min3(y0, y1, y2) + 0xF) >> 4;
                let max_x = (max3(x0, x1, x2) + 0xF) >> 4;
                let max_y = (max3(y0, y1, y2) + 0xF) >> 4;

                // Clip against framebuffer
                let min_x = na::clamp(min_x, 0, w);
                let min_y = na::clamp(min_y, 0, h);
                let max_x = na::clamp(max_x, 0, w);
                let max_y = na::clamp(max_y, 0, h);

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
                        let w0 = (x1 - x0) * (yf - y0) - (y1 - y0) * (xf - x0) + e0add;
                        let w1 = (x2 - x1) * (yf - y1) - (y2 - y1) * (xf - x1) + e1add;
                        let w2 = (x0 - x2) * (yf - y2) - (y0 - y2) * (xf - x2) + e2add;
                        if w0 > 0 && w1 > 0 && w2 > 0 {
                            // The cross product from the edge function not only tells us
                            // which side we're on, but also the area of parallelogram
                            // formed by the two vectors. We're basically getting twice
                            // the area of one of the three triangles splitting the
                            // original triangle around the raster position. Those are
                            // already barycentric coordinates, we just need to normalize
                            // them with the triangle area we already computed with the
                            // cross product for the backface culling test
                            let b0 = (w0 - e0add) as f32 * inv_tri_a2;
                            let b1 = (w1 - e1add) as f32 * inv_tri_a2;
                            let b2 = (w2 - e2add) as f32 * inv_tri_a2;

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

                            // Interpolate color (TODO: perspective correction)
                            /*
                            let w = 1.0 / ((1.0/vtx0.p.w) * b1 +
                                           (1.0/vtx1.p.w) * b2 +
                                           (1.0/vtx2.p.w) * b0);
                            let cf_persp = ((*c0/vtx0.p.w) * b1 +
                                            (*c1/vtx1.p.w) * b2 +
                                            (*c2/vtx2.p.w) * b0) * w;
                            let cf_no_persp = *c0 * b1 + *c1 * b2 + *c2 * b0;
                            let cf = if y < h / 2 { cf_persp } else { cf_no_persp };
                            */
                            let cf = *c0 * b1 + *c1 * b2 + *c2 * b0;

                            // Write color
                            unsafe {
                                * fb.offset(idx) = rgbf_to_abgr32(cf.x, cf.y, cf.z);
                            }
                        }
                    }
                }
            }
        }
    }
}

