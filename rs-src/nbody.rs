
use std::sync::Mutex;
use std::ptr;
use rand;
use rand::distributions::{IndependentSample, Range};
use std::f32::consts;

#[derive(Clone, Copy)]
struct Particle {
    px : f32,
    py : f32,
    vx : f32,
    vy : f32,
    m  : f32
}

fn uniform_sample_disk(x: &mut f32, y: &mut f32) {
    let r = x.sqrt();
    let theta = 2.0 * consts::PI * (* y);
    * x = r * theta.cos();
    * y = r * theta.sin();
}

lazy_static! {
    static ref PARTICLES: Mutex<Vec<Particle>> = {
        let mut v = Vec::new();

        /*
        v.push(Particle { px: 0.5, py: 0.5, vx: 0.0, vy: 0.0, m: 1000.0 });

        v.push(Particle { px: -8.23656 , py: 1.78778 , vx: -4.61142 , vy: -31.2847, m: 10.0 });
        v.push(Particle { px: -4.12657, py:  -2.99729, vx:  19.069, vy:  -25.2264, m: 10.0 });
        v.push(Particle { px: -6.00204, py: 27.3733, vx: -30.7359, vy: -7.4366, m: 10.0 });
        */

        let mut rng = rand::thread_rng();
        let vel = Range::new(-3.5, 3.5);
        let mass = Range::new(0.1, 1.5);
        let u = Range::new(0.0, 1.0);

        for _ in 0..1000 {
            let mut x : f32 = u.ind_sample(&mut rng);
            let mut y : f32 = u.ind_sample(&mut rng);
            uniform_sample_disk(&mut x, &mut y);
            x *= 20.0;
            y *= 20.0;

            v.push(Particle { px: x,
                              py: y,
                              vx: vel.ind_sample(&mut rng),
                              vy: vel.ind_sample(&mut rng),
                              m:  mass.ind_sample(&mut rng) });
        }

        Mutex::new(v)
    };
}

#[no_mangle]
pub extern fn nb_step() -> () {
    let mut particles_mtx = PARTICLES.lock().unwrap();

    // Compute forces
    let forces;
    {
        let particles = &(* particles_mtx);

        #[derive(Clone)]
        struct Force {
            fx : f32,
            fy : f32
        }
        let mut mforces = Vec::new();
        mforces.resize(particles.len(), Force { fx: 0.0, fy: 0.0 });

        for i in 0..particles.len() {
            let a = &particles[i];

            for j in 0..particles.len() {
                if i == j { continue; }

                let b = &particles[j];

                let dx = b.px - a.px;
                let dy = b.py - a.py;
                let dist = (dx * dx + dy * dy).sqrt();

                let eps = 0.00001; // Softening factor, prevent singularities
                let f = a.m * b.m / (dist * dist + eps);

                mforces[i].fx += f * dx; // / dist;
                mforces[i].fy += f * dy; // / dist;
            }
        }

        forces = mforces;
    }

    // Update positions and velocities
    {
        let mut particles = &mut (* particles_mtx);

        let dt = 0.01;

        for i in 0..particles.len() {
            particles[i].vx += dt * forces[i].fx / particles[i].m;
            particles[i].vy += dt * forces[i].fy / particles[i].m;

            particles[i].px += dt * particles[i].vx;
            particles[i].py += dt * particles[i].vy;
        }
    }
}

#[no_mangle]
pub extern fn nb_draw(w: i32, h: i32, fb: *mut u32) -> () {
    // Clear background
    unsafe { ptr::write_bytes(fb, 0, (w * h) as usize); }

    // Specify viewport over simulation to be mapped to the framebuffer
    let vp_wdh   = 100.0;
    let vp_org_x = 0.0;
    let vp_org_y = 0.0;

    // Keep full width of the viewport visible, adjust height as needed
    let aspect = h as f32 / w as f32;

    // Framebuffer viewport in simulation dimensions
    let x1 =  vp_org_x - vp_wdh / 2.0;
    let y1 = (vp_org_y - vp_wdh / 2.0) * aspect;
    let x2 =  vp_org_x + vp_wdh / 2.0;
    let y2 = (vp_org_y + vp_wdh / 2.0) * aspect;

    // Scale factor for translating from simulation to framebuffer
    let vpw    = x2 - x1;
    let vph    = y2 - y1;
    let scalex = (1.0 / vpw) * w as f32;
    let scaley = (1.0 / vph) * h as f32;

    let particles = PARTICLES.lock().unwrap();
    for p in &(*particles) {
        // Translate from simulation to viewport coordinates
        let x = ((p.px - x1) * scalex) as i32;
        let y = ((p.py - y1) * scaley) as i32;

        // Bounds check
        if x < 0 || x >= w || y < 0 || y >= h { continue; }

        // Draw particle
        let idx = x + y * w;
        unsafe {
            * fb.offset(idx as isize) = 0x00FFFFFF;
        }
    }

    // Debug draw center cross
    unsafe {
        * fb.offset((w / 2 + 0 + (h / 2 + 0) * w) as isize) = 0x00FF00FF;
        * fb.offset((w / 2 + 1 + (h / 2 + 0) * w) as isize) = 0x00FF00FF;
        * fb.offset((w / 2 + 0 + (h / 2 + 1) * w) as isize) = 0x00FF00FF;
        * fb.offset((w / 2 - 1 + (h / 2 + 0) * w) as isize) = 0x00FF00FF;
        * fb.offset((w / 2 + 0 + (h / 2 - 1) * w) as isize) = 0x00FF00FF;
    }
}

