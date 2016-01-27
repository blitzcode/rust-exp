
use std::f32::consts;

#[no_mangle]
pub extern fn sine_scroller(w: i32, h: i32, fb: *mut u32, tick: f64) -> () {
    for y in 0..h {
        let sy = (((y as f32) / 64.0 + tick as f32) * 2.0 * consts::PI).sin();
        for x in 0..w {
            let sx = (((x as f32) / 64.0 + tick as f32) * 2.0 * consts::PI).sin();
            let gray = (((sx + 1.0) * 0.5) * ((sy + 1.0) * 0.5) * 255.0) as u32;
            let idx = x + y * w;
            unsafe {
                *fb.offset(idx as isize) = gray | gray << 8 | gray << 16;
            }
        }
    }
}

