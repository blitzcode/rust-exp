
use std::sync::Mutex;

lazy_static! {
    static ref MESH: Mutex<Vec<f32>> = {
        Mutex::new(Vec::new())
    };
}

#[no_mangle]
pub extern fn rast_draw(tick: f64, w: i32, h: i32, fb: *mut u32) -> () {
    let mesh = MESH.lock().unwrap();
}

