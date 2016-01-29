
use std::sync::Mutex;
use rand;
use rand::Rng;
use std::ptr;

#[derive(Clone)]
struct Particle {
    px : f32,
    py : f32,
    vx : f32,
    vy : f32
}

lazy_static! {
    static ref PARTICLES: Mutex<Vec<Particle>> = {
        let mut v = Vec::new();
        v.resize(1024, Particle { px: 0.0, py: 0.0, vx: 0.0, vy: 0.0 });
        Mutex::new(v)
    };
}

#[no_mangle]
pub extern fn nb_step() -> () {
}

#[no_mangle]
pub extern fn nb_draw(w: i32, h: i32, fb: *mut u32) -> () {
}

