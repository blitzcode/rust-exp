
use std::sync::Mutex;
use nalgebra::*;
use std::path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;
use std::f32;

lazy_static! {
    static ref CORNELL_MESH: Mutex<Mesh> = {
        Mutex::new(
            load_mesh(&String::from("data/cornell_radiosity.dat"), MeshFileType::XyzRgbXx))
    };
}

lazy_static! {
    static ref HEAD_MESH: Mutex<Mesh> = {
        Mutex::new(
            load_mesh(&String::from("data/head_ao.dat"), MeshFileType:: XyzNxnynzAoao))
    };
}

#[derive(Clone, Copy)]
struct Vertex {
    p:   Vec3<f32>,
    n:   Vec3<f32>,
    col: Vec3<f32>
}

impl Vertex {
    fn new(px: f32, py: f32, pz: f32,
           nx: f32, ny: f32, nz: f32,
           r:  f32, g:  f32, b:  f32) -> Vertex {
        Vertex {
            p:   Vec3::new(px, py, pz),
            n:   Vec3::new(nx, ny, nz),
            col: Vec3::new(r , g , b)
        }
    }
}

// Fully inlined triangle representation
#[derive(Clone, Copy)]
struct Triangle {
    v0: Vertex,
    v1: Vertex,
    v2: Vertex
}

impl Triangle {
    fn new(v0: &Vertex, v1: &Vertex, v2: &Vertex) -> Triangle {
        Triangle { v0: *v0, v1: *v1, v2: *v2 }
    }

    fn face_normal(&self) -> Vec3<f32> {
        (self.v1.p - self.v0.p).cross(&(self.v2.p - self.v0.p)).normalize()
    }
}

struct Mesh {
    tri:      Vec<Triangle>,
    aabb_min: Vec3<f32>,
    aabb_max: Vec3<f32>
}

impl Mesh {
    fn new(tri: Vec<Triangle>) -> Mesh {
        // Compute AABB
        let mut aabb_min = Vec3::new(f32::MAX, f32::MAX, f32::MAX);
        let mut aabb_max = Vec3::new(f32::MIN, f32::MIN, f32::MIN);
        for t in &tri {
            for v in &[t.v0.p, t.v1.p, t.v2.p] {
                aabb_min.x = if aabb_min.x < v.x { aabb_min.x } else { v.x };
                aabb_min.y = if aabb_min.y < v.y { aabb_min.y } else { v.y };
                aabb_min.z = if aabb_min.z < v.z { aabb_min.z } else { v.z };
                aabb_max.x = if aabb_max.x > v.x { aabb_max.x } else { v.x };
                aabb_max.y = if aabb_max.y > v.y { aabb_max.y } else { v.y };
                aabb_max.z = if aabb_max.z > v.z { aabb_max.z } else { v.z };
            }
        }

        Mesh { tri: tri, aabb_min: aabb_min, aabb_max: aabb_max }
    }
}

// We either have 'Px Py Pz ColR ColB ColG UVx UVy' with bogus UVs and radiosity data
// in the color channel or 'Px Py Pz Nx Ny Nz UVx UVy' with ambient occlusion in both
// texture coordinates
#[derive(PartialEq)]
enum MeshFileType { XyzRgbXx, XyzNxnynzAoao }

fn load_mesh(file_name: &String, mesh_file_type: MeshFileType) -> Mesh {
    // Load a text format mesh from disk

    let path = path::Path::new(file_name);
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
                    components.push(words.next().unwrap().parse::<u32>().unwrap());
                }
                idx.push((components[0], components[1], components[2]));
                // Done?
                if idx.len() * 3 == idx_cnt as usize { break }
            }
            None => panic!("load_mesh(): EOF while parsing indices: {}", display)
        }
    }

    // Assemble triangle vector and mesh
    let mut triangles = Vec::new();
    for tri_idx in idx {
        let mut ntri = Triangle::new(&vtx[tri_idx.0 as usize],
                                     &vtx[tri_idx.1 as usize],
                                     &vtx[tri_idx.2 as usize]);
        if mesh_file_type == MeshFileType::XyzRgbXx {
            // Set vertex normals from face normal
            let n = ntri.face_normal();
            ntri.v0.n = n;
            ntri.v1.n = n;
            ntri.v2.n = n;
        }
        triangles.push(ntri);
    }
    let mesh = Mesh::new(triangles);

    // Print some mesh information
    println!("load_mesh(): Loaded {} tri and {} vtx from {}, AABB ({}, {}, {}) - ({}, {}, {})",
        mesh.tri.len(), vtx.len(), display,
        mesh.aabb_min.x, mesh.aabb_min.y, mesh.aabb_min.z,
        mesh.aabb_max.x, mesh.aabb_max.y, mesh.aabb_max.z);

    mesh
}

#[no_mangle]
pub extern fn rast_draw(tick: f64, w: i32, h: i32, fb: *mut u32) -> () {
    let cornell_mesh = CORNELL_MESH.lock().unwrap();
    let head_mesh = HEAD_MESH.lock().unwrap();

    // Background gradient
    // let start = Vec3::new(1.0, 0.4, 0.0);
    // let end   = Vec3::new(0.0, 0.5, 0.5);
    let start = Vec3::new(0.3, 0.3, 0.3);
    let end   = Vec3::new(0.7, 0.7, 0.7);
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

fn rgbf_to_abgr32(r: f32, g: f32, b: f32) -> u32 {
    let r8 = (clamp(r, 0.0, 1.0) * 255.0) as u32;
    let g8 = (clamp(g, 0.0, 1.0) * 255.0) as u32;
    let b8 = (clamp(b, 0.0, 1.0) * 255.0) as u32;
    r8 | (g8 << 8) | (b8 << 16)
}

