
use std::sync::Mutex;
use std::ptr;
use rand;
use rand::distributions::{IndependentSample, Range};
use std::f32::consts;
use std::cmp::min;

#[derive(Clone, Copy)]
struct Particle {
    px : f32,
    py : f32,
    vx : f32,
    vy : f32,
    m  : f32
}

lazy_static! {
    static ref PARTICLES: Mutex<Vec<Particle>> = {
        Mutex::new(Vec::new())
    };
}

#[no_mangle]
pub extern fn nb_random_disk(num_particles: i32) -> () {
    // Place particles with random position and velocity in a disk in the center of the viewport

    let mut particles = PARTICLES.lock().unwrap();
    particles.clear();

    let mut rng = rand::thread_rng();
    let vel  = Range::new(-3.5, 3.5);
    let mass = Range::new( 0.1, 1.5);
    let u    = Range::new( 0.0, 1.0);

    for _ in 0..num_particles {
        let mut x : f32 = u.ind_sample(&mut rng);
        let mut y : f32 = u.ind_sample(&mut rng);
        uniform_sample_disk(&mut x, &mut y);
        x *= 20.0;
        y *= 20.0;

        particles.push(Particle { px: x,
                                  py: y,
                                  vx: vel.ind_sample(&mut rng),
                                  vy: vel.ind_sample(&mut rng),
                                  m:  mass.ind_sample(&mut rng) });
    }
}

fn uniform_sample_disk(x: &mut f32, y: &mut f32) {
    let r = x.sqrt();
    let theta = 2.0 * consts::PI * (* y);
    * x = r * theta.cos();
    * y = r * theta.sin();
}

#[no_mangle]
pub extern fn nb_stable_orbits(num_particles: i32, rmin: f32, rmax: f32) -> () {
    // Generate a bunch of 'planets' with a circular orbit around a 'sun'
    //
    // References:
    //
    // http://www.cs.cmu.edu/~./ph/859E/src/nbody/circ.p2
    // www.cs.cmu.edu/~./ph/859E/src/nbody/nbody.cc

    let mut particles = PARTICLES.lock().unwrap();
    particles.clear();

    let sun_mass    = 1000.0;
    let planet_mass = 1.0;
    let g : f32     = 1.0;
    let speed       = (g * sun_mass).sqrt();

    let mut rng     = rand::thread_rng();
    let u           = Range::new(0.0, 1.0);

    particles.push( Particle { px: 0.0, py: 0.0, vx: 0.0, vy: 0.0, m: sun_mass } );

    for _ in 0..num_particles {
        let r     = (rmax - rmin) * u.ind_sample(&mut rng) + rmin;
        let theta = 2.0 * consts::PI * u.ind_sample(&mut rng);
        particles.push( Particle { px: r * theta.cos(),
                                   py: r * theta.sin(),
                                   vx: -speed * theta.sin(),
                                   vy:  speed * theta.cos(),
                                   m: planet_mass });
    }
}

#[no_mangle]
pub extern fn nb_step(dt : f32) -> () {
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

                let eps = 0.0001; // Softening factor, prevent singularities
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
    // If writing scattered points to write combined memory turns out to be a problem,
    // enabled buffering
    let buffer = false;

    if !buffer {
        // Clear background
        unsafe { ptr::write_bytes(fb, 0, (w * h) as usize); }
    }

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

    // Optionally allocate buffer
    let buf_ptr;
    if buffer {
        let mut buf = Vec::new();
        buf.resize((w * h) as usize, 0u32);
        buf_ptr = buf.as_mut_ptr();
    } else {
        buf_ptr = fb;
    }

    let particles = PARTICLES.lock().unwrap();

    for p in &(*particles) {
        // Translate from simulation to viewport coordinates
        let x = (p.px - x1) * scalex;
        let y = (p.py - y1) * scaley;

        // Draw semi-transparent particle with slightly more transparent tail
        for i in 0..2 {
            let xo;
            let yo;
            let col;
            if i == 0 {
                xo = x as i32;
                yo = y as i32;
                col = 0x00404040;
            } else {
                // Tail
                let len = 1.0 / (p.vx * p.vx + p.vy * p.vy).sqrt();
                let vnx = p.vx * len;
                let vny = p.vy * len;
                xo = (x + 0.3 - vnx) as i32;
                yo = (y + 0.3 - vny) as i32;
                col = 0x00303030;
            }

            // Bounds check
            if xo < 0 || xo >= w || yo < 0 || yo >= h { continue; }

            // Draw particle
            let idx = xo + yo * w;
            unsafe {
                * buf_ptr.offset(idx as isize) =
                    add_abgr32(* buf_ptr.offset(idx as isize), col);
            }
        }
    }

    // Debug draw center cross
    unsafe {
        * buf_ptr.offset((w / 2 + 0 + (h / 2 + 0) * w) as isize) = 0x00FF00FF;
        * buf_ptr.offset((w / 2 + 1 + (h / 2 + 0) * w) as isize) = 0x00FF00FF;
        * buf_ptr.offset((w / 2 + 0 + (h / 2 + 1) * w) as isize) = 0x00FF00FF;
        * buf_ptr.offset((w / 2 - 1 + (h / 2 + 0) * w) as isize) = 0x00FF00FF;
        * buf_ptr.offset((w / 2 + 0 + (h / 2 - 1) * w) as isize) = 0x00FF00FF;
    }

    // Output in one contiguous copy
    if buffer {
        unsafe { ptr::copy_nonoverlapping(buf_ptr, fb, (w * h) as usize); }
    }
}

fn add_abgr32(c1 : u32, c2 : u32) -> u32 {
    // Add two 32 bit ABGR values together
    //
    // TODO: In our case A is always 0 anyway, we could just add the components
    //       together and combine them without any of the shifts

    let a1 = (c1 & 0xFF000000) >> 24;
    let b1 = (c1 & 0x00FF0000) >> 16;
    let g1 = (c1 & 0x0000FF00) >>  8;
    let r1 = (c1 & 0x000000FF) >>  0;

    let a2 = (c2 & 0xFF000000) >> 24;
    let b2 = (c2 & 0x00FF0000) >> 16;
    let g2 = (c2 & 0x0000FF00) >>  8;
    let r2 = (c2 & 0x000000FF) >>  0;

    let ar = min(255, a1 + a2);
    let gr = min(255, g1 + g2);
    let br = min(255, b1 + b2);
    let rr = min(255, r1 + r2);

    (ar << 24) | (gr << 16) | (br << 8) | (rr << 0)
}

