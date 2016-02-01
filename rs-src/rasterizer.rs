
use std::sync::Mutex;
use nalgebra::*;
use std::path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;

lazy_static! {
    static ref CORNELL_MESH: Mutex<Vec<Triangle>> = {
        Mutex::new(
            load_mesh(&String::from("data/cornell_radiosity.dat"), MeshFileType::XyzRgbXx))
    };
}

lazy_static! {
    static ref HEAD_MESH: Mutex<Vec<Triangle>> = {
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

// We either have 'Px Py Pz ColR ColB ColG UVx UVy' with bogus UVs and radiosity data
// in the color channel or 'Px Py Pz Nx Ny Nz UVx UVy' with ambient occlusion in both
// texture coordinates
#[derive(PartialEq)]
enum MeshFileType { XyzRgbXx, XyzNxnynzAoao }

fn load_mesh(file_name: &String, mesh_file_type: MeshFileType) -> Vec<Triangle> {
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

    // Assemble triangle vector
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

    // println!("load_mesh(): Loaded {} tri and {} vtx from {}",
    //     triangles.len(), vtx.len(), display);

    triangles
}

#[no_mangle]
pub extern fn rast_draw(tick: f64, w: i32, h: i32, fb: *mut u32) -> () {
    let cornell_mesh = CORNELL_MESH.lock().unwrap();
    let head_mesh = HEAD_MESH.lock().unwrap();
}

