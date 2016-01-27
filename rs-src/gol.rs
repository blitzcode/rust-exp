
use std::sync::Mutex;
use rand;
use rand::Rng;
use std::ptr;

static GRID_WDH : i32 = 256;

lazy_static! {
    static ref GRID: Mutex<Vec<u8>> = {
        let mut v = Vec::new();
        v.resize((GRID_WDH * GRID_WDH) as usize, 0);
        Mutex::new(v)
    };
}

#[no_mangle]
pub extern fn gol_randomize() -> () {
    let mut grid = GRID.lock().unwrap();
    let mut rng = rand::thread_rng();

    for y in 0..GRID_WDH {
        for x in 0..GRID_WDH {
            let idx = (x + y * GRID_WDH) as usize;
            grid[idx] = if rng.gen() { 1 } else { 0 };
        }
    }
}

#[no_mangle]
pub extern fn gol_step() -> () {

    // Allocate new grid without wasting time setting it to any value
    let grid_size = (GRID_WDH * GRID_WDH) as usize;
    let mut new_grid = Vec::with_capacity(grid_size);
    unsafe { new_grid.set_len(grid_size); }

    let mut grid = GRID.lock().unwrap();

    for y in 0..GRID_WDH {
        for x in 0..GRID_WDH {
            let torus_idx = |x: i32, y: i32| -> u8 {
                let wrapx = if x < 0 { GRID_WDH - 1 } else { if x > GRID_WDH - 1 { 0 } else { x } };
                let wrapy = if y < 0 { GRID_WDH - 1 } else { if y > GRID_WDH - 1 { 0 } else { y } };
                let idx = wrapx + wrapy * GRID_WDH;
                grid[idx as usize]
            };

            let idx = (x + y * GRID_WDH) as usize;
            let alive = grid[idx];
            let alive_nb = torus_idx(x + 1, y    ) +
                           torus_idx(x    , y + 1) +
                           torus_idx(x - 1, y    ) +
                           torus_idx(x    , y - 1) +
                           torus_idx(x + 1, y + 1) +
                           torus_idx(x - 1, y - 1) +
                           torus_idx(x + 1, y - 1) +
                           torus_idx(x - 1, y + 1);

            new_grid[idx] = if alive_nb == 3 || (alive == 1 && alive_nb == 2) { 1 } else { 0 };
        }
    }

    * grid = new_grid;
}

#[no_mangle]
pub extern fn gol_draw(w: i32, h: i32, fb: *mut u32) -> () {

    // Clear background
    unsafe { ptr::write_bytes(fb, 64, (w * h) as usize); }

    let grid = GRID.lock().unwrap();

    // Center
    let xoffs = w / 2 - GRID_WDH / 2;
    let yoffs = h / 2 - GRID_WDH / 2;

    for y in 0..GRID_WDH {
        for x in 0..GRID_WDH {
            let idx_fb = (xoffs + x) + (yoffs + y) * w;

            // Out of bounds check for the FB
            if idx_fb < 0 || idx_fb > w * h - 1 { continue; }

            let idx_grid = x + y * GRID_WDH;

            unsafe {
                * fb.offset(idx_fb as isize) =
                    if grid[idx_grid as usize] == 1 { 0x00FFFFFF } else { 0 };
            }
        }
    }
}

#[no_mangle]
pub extern fn gol_set_pattern(w: i32, h: i32, pat: *mut u8) -> () {

    let mut grid = GRID.lock().unwrap();

    // Clear grid
    let grid_size = GRID_WDH * GRID_WDH;
    grid.clear();
    grid.resize(grid_size as usize, 0);

    // Center
    let xoffs = GRID_WDH / 2 - w / 2;
    let yoffs = GRID_WDH / 2 - h / 2;

    for y in 0..h {
        for x in 0..w {
            let idx_grid = (xoffs + x) + (yoffs + y) * GRID_WDH;

            // Out of bounds check for the grid
            if idx_grid < 0 || idx_grid > grid_size - 1 { continue; }

            let idx_pat = x + y * w;
            unsafe { grid[idx_grid as usize] = * pat.offset(idx_pat as isize); }
        }
    }
}

