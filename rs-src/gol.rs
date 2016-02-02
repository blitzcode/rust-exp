
use std::sync::Mutex;
use rand;
use rand::Rng;
use std::ptr;
use std::thread;

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
    let mut rng  = rand::thread_rng();

    for y in 0..GRID_WDH {
        for x in 0..GRID_WDH {
            let idx = (x + y * GRID_WDH) as usize;
            grid[idx] = if rng.gen() { 1 } else { 0 };
        }
    }
}

#[no_mangle]
pub extern fn gol_step(nthreads : i32) -> () {
    // Allocate new grid without wasting time setting it to any value
    let grid_size    = (GRID_WDH * GRID_WDH) as usize;
    let mut new_grid = Vec::with_capacity(grid_size);
    unsafe { new_grid.set_len(grid_size); }
    let new_grid_ptr = new_grid.as_mut_ptr();

    let mut grid = GRID.lock().unwrap();
    let grid_ptr = grid.as_mut_ptr();

    // Compute the four borders
    for side in 0..4 {
        let mut x;
        let mut y;
        let xincr;
        let yincr;
        match side {
            0 => { x = 0           ; y = 0           ; xincr = 1; yincr = 0; },
            1 => { x = 0           ; y = GRID_WDH - 1; xincr = 1; yincr = 0; },
            2 => { x = 0           ; y = 0           ; xincr = 0; yincr = 1; },
            3 => { x = GRID_WDH - 1; y = 0           ; xincr = 0; yincr = 1; },
            _ => panic!("invalid side")
        }

        while x < GRID_WDH && y < GRID_WDH {
            // We're at the boundary, wrap
            let torus_idx = |x: i32, y: i32| -> u8 {
                let wrapx = if x < 0 { GRID_WDH - 1 } else { if x > GRID_WDH - 1 { 0 } else { x } };
                let wrapy = if y < 0 { GRID_WDH - 1 } else { if y > GRID_WDH - 1 { 0 } else { y } };
                let idx   = wrapx + wrapy * GRID_WDH;
                unsafe { * grid_ptr.offset(idx as isize) }
            };

            // 2D indexing, faster to wrap
            let idx      = (x + y * GRID_WDH) as isize;
            let alive    = unsafe { * grid_ptr.offset(idx as isize) };
            let alive_nb = torus_idx(x + 1, y    ) +
                           torus_idx(x    , y + 1) +
                           torus_idx(x - 1, y    ) +
                           torus_idx(x    , y - 1) +
                           torus_idx(x + 1, y + 1) +
                           torus_idx(x - 1, y - 1) +
                           torus_idx(x + 1, y - 1) +
                           torus_idx(x - 1, y + 1);

            unsafe {
                * new_grid_ptr.offset(idx as isize) =
                    if alive_nb == 3 || (alive == 1 && alive_nb == 2) { 1 } else { 0 };
            }

            x += xincr;
            y += yincr;
        }
    }

    // Compute interior
    if nthreads == 1 {
        // Single-threaded version. There's quite a bit of overhead for the threads, make
        // sure no performance is lost

        // TODO: Duplicate code between serial / parallel version
        for y in 1..GRID_WDH - 1 {
            for x in 1..GRID_WDH - 1 {
                // 1D indexing, no wrapping needed
                let idx = x + y * GRID_WDH;
                let alive = unsafe { * grid_ptr.offset(idx as isize) };
                let alive_nb = unsafe {
                    * grid_ptr.offset((idx + 1           ) as isize) +
                    * grid_ptr.offset((idx - 1           ) as isize) +
                    * grid_ptr.offset((idx     + GRID_WDH) as isize) +
                    * grid_ptr.offset((idx     - GRID_WDH) as isize) +
                    * grid_ptr.offset((idx + 1 + GRID_WDH) as isize) +
                    * grid_ptr.offset((idx + 1 - GRID_WDH) as isize) +
                    * grid_ptr.offset((idx - 1 + GRID_WDH) as isize) +
                    * grid_ptr.offset((idx - 1 - GRID_WDH) as isize)
                };

                unsafe {
                    * new_grid_ptr.offset(idx as isize) =
                        if alive_nb == 3 || (alive == 1 && alive_nb == 2) { 1 } else { 0 };
                }
            }
        }
    } else {
        // Multi-threaded version

        let threads: Vec<_> = (0..nthreads).map(|i| {
            // Vertical slice to be processed by the current thread
            let range    = (GRID_WDH - 2) / nthreads;
            let seg_low  = 1 + range * i;
            let seg_high = if i == nthreads - 1 { GRID_WDH - 1 } else { 1 + range * (i + 1) };

            // Allow sharing of pointers with our threads
            struct SendPtr {
                ptr : *mut u8
            }
            unsafe impl Send for SendPtr { }
            let grid_ptr_send     = SendPtr { ptr: grid_ptr     };
            let new_grid_ptr_send = SendPtr { ptr: new_grid_ptr };

            // TODO: Thread creation seems expensive compared to our computation, need to
            //       re-use threads between steps for more scalability
            thread::spawn(move || {
                // Unwrap
                let grid_ptr     = grid_ptr_send.ptr;
                let new_grid_ptr = new_grid_ptr_send.ptr;

                for y in seg_low..seg_high {
                    for x in 1..GRID_WDH - 1 {
                        // 1D indexing, no wrapping needed
                        let idx      = x + y * GRID_WDH;
                        let alive    = unsafe { * grid_ptr.offset(idx as isize) };
                        let alive_nb = unsafe {
                            * grid_ptr.offset((idx + 1           ) as isize) +
                            * grid_ptr.offset((idx - 1           ) as isize) +
                            * grid_ptr.offset((idx     + GRID_WDH) as isize) +
                            * grid_ptr.offset((idx     - GRID_WDH) as isize) +
                            * grid_ptr.offset((idx + 1 + GRID_WDH) as isize) +
                            * grid_ptr.offset((idx + 1 - GRID_WDH) as isize) +
                            * grid_ptr.offset((idx - 1 + GRID_WDH) as isize) +
                            * grid_ptr.offset((idx - 1 - GRID_WDH) as isize)
                        };

                        unsafe {
                            * new_grid_ptr.offset(idx as isize) =
                                if alive_nb == 3 || (alive == 1 && alive_nb == 2) { 1 } else { 0 };
                        }
                    }
                }
            })
        }).collect();

        for thread in threads {
            thread.join().unwrap();
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
    // Place specified pattern in the center of an empty grid
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

