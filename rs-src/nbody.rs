
use std::sync::Mutex;
use std::ptr;
use rand;
use rand::distributions::{IndependentSample, Range};
use std::f32::consts;
use std::f32;
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
pub extern fn nb_num_particles() -> i32 {
    PARTICLES.lock().unwrap().len() as i32
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

    for _ in 0..num_particles - 1 {
        let r     = (rmax - rmin) * u.ind_sample(&mut rng) + rmin;
        let theta = 2.0 * consts::PI * u.ind_sample(&mut rng);
        particles.push( Particle { px: r * theta.cos(),
                                   py: r * theta.sin(),
                                   vx: -speed * theta.sin(),
                                   vy:  speed * theta.cos(),
                                   m : planet_mass });
    }
}

#[no_mangle]
pub extern fn nb_step_brute_force(dt : f32) -> () {
    // Brute-force O(N^2) simulation algorithm. This is completely unoptimized, we even
    // compute the forces twice for each particle pair and do none of the obvious
    // arithmetic optimizations like factoring out one of the particle masses etc.
    //
    // References:
    //
    // http://http.developer.nvidia.com/GPUGems3/gpugems3_ch31.html
    // http://physics.princeton.edu/~fpretori/Nbody/code.htm

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

                let (fx_add, fy_add) = force(a.px, a.py, a.m, b.px, b.py, b.m);
                mforces[i].fx += fx_add;
                mforces[i].fy += fy_add;
            }
        }

        forces = mforces;
    }

    // Update positions and velocities
    {
        let mut particles = &mut (* particles_mtx);

        for i in 0..particles.len() {
            // F = ma, a = F / m
            particles[i].vx += dt * forces[i].fx / particles[i].m;
            particles[i].vy += dt * forces[i].fy / particles[i].m;

            particles[i].px += dt * particles[i].vx;
            particles[i].py += dt * particles[i].vy;
        }
    }
}

fn force(px1: f32, py1: f32, m1: f32, px2: f32, py2: f32, m2: f32) -> (f32, f32) {
    // Compute forces between particle pair
    //
    // References:
    //
    // https://en.wikipedia.org/wiki/Newton's_law_of_universal_gravitation#Vector_form

    let dx = px2 - px1;
    let dy = py2 - py1;
    let dist_sq = dx * dx + dy * dy;
    // let dist = dist_sq.sqrt();

    let eps = 0.0001; // Softening factor, prevent singularities
    let f = m1 * m2 / (dist_sq + eps);

    // (f * dx / dist, f * dy / dist)
    (f * dx, f * dy)
}

#[no_mangle]
pub extern fn nb_step_barnes_hut(theta : f32, dt : f32) -> () {
    // Hierarchical O(n log n) simulation algorithm using the Barnes-Hut approximation algorithm
    //
    // References:
    //
    // https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation
    // http://arborjs.org/docs/barnes-hut

    // With a zero Theta we're forced to compute all N^2 interactions anyway, might
    // as well not build the tree and just switch to the brute force algorithm
    if theta == 0.0 {
        nb_step_brute_force(dt);
        return;
    }

    // Quad tree
    #[derive(Copy, Clone)]
    enum Quadrant { UL, UR, LL, LR }
    struct Node {
        // AABB (TODO: Only need this at construction time, could just store width instead)
        x1 : f32, y1 : f32, x2 : f32, y2 : f32,
        // Center of mass + total mass for interior nodes,
        // contained particle exterior ones
        px : f32, py : f32, m : f32,
        // Child nodes (TODO: Just store indices, half the storage, less scattered memory access)
        children: Option<Box<[Node; 4]>>
    }
    impl Node {
        fn new(x1: f32, y1: f32, x2: f32, y2: f32) -> Node {
            Node {
                x1: x1, y1: y1, x2: x2, y2: y2,
                px: 0.0, py: 0.0, m: 0.0,
                children: None
            }
        }

        fn has_children(&self) -> bool { self.children.is_some() }

        fn insert(&mut self, px: f32, py: f32, m: f32, depth : u32) {
            if depth > 100 { panic!("Node::insert() - infinite recursion") }
            if self.has_children() {
                // Interior node, accumulate mass and keep traversing
                self.add_mass(px, py, m);
                let quadrant = self.quadrant_from_point(px, py);
                match self.children {
                    Some(ref mut children) => {
                        children[quadrant as usize].insert(px, py, m, depth + 1)
                    }
                    // TODO: We could avoid this awkward case if we just pattern matched
                    //       instead of using has_children() in the first place, but
                    //       convincing Rust's borrow checker to let us do this is
                    //       another matter
                    None => { panic!("Node::insert() - children missing") }
                }
            } else {
                if self.m == 0.0 {
                    // No mass means empty exterior node, just insert particle here
                    self.add_mass(px, py, m);
                } else {
                    // Non-empty exterior node. Before we continue to insert we first need
                    // to split it by clearing it, create its children and then
                    // insert the original particle back
                    let px_original = self.px;
                    let py_original = self.py;
                    let m_original  = self.m;
                    self.px = 0.0;
                    self.py = 0.0;
                    self.m  = 0.0;
                    self.create_children();
                    self.insert(px_original, py_original, m_original, depth + 1);
                    // Now keep inserting
                    self.insert(px, py, m, depth + 1);
                }
            }
        }

        fn create_children(&mut self) {
            assert!(self.has_children() == false);
            let cx = (self.x1 + self.x2) * 0.5;
            let cy = (self.y1 + self.y2) * 0.5;
            self.children = Some(Box::new([
                Node::new(self.x1, cy     , cx     , self.y2), // UL
                Node::new(cx     , cy     , self.x2, self.y2), // UR
                Node::new(self.x1, self.y1, cx     , cy     ), // LL
                Node::new(cx     , self.y1, self.x2, cy     )  // LR
            ]))
        }

        fn add_mass(&mut self, px: f32, py: f32, m: f32) {
            assert!(m > 0.0);
            if self.m == 0.0 {
                // Special case for empty nodes. The whole center of gravity computation
                // introduces small floating-point errors, making our 'i /= j' check based
                // on particle positions fail during force computations
                self.px = px;
                self.py = py;
                self.m  = m;
            } else {
                // Add particle mass, update center of gravity
                let inv_msum = 1.0 / (self.m + m);
                self.px = (self.px * self.m + px * m) * inv_msum;
                self.py = (self.py * self.m + py * m) * inv_msum;
                self.m += m;
            }
        }

        fn quadrant_from_point(&self, x: f32, y: f32) -> Quadrant {
            // We assume the point is inside the AABB
            let cx = (self.x1 + self.x2) * 0.5;
            let cy = (self.y1 + self.y2) * 0.5;
            if y < cy {
                if x < cx { Quadrant::LL } else { Quadrant::LR }
            } else {
                if x < cx { Quadrant::UL } else { Quadrant::UR }
            }
        }

        fn compute_force(&self, px: f32, py: f32, m: f32, theta: f32) -> (f32, f32) {
            // Compute the forces acting on the specified particle. Use theta as a cutoff
            // criteria for when to switch to single-particle approximations of sub trees
            let mut fx = 0.0;
            let mut fy = 0.0;
            match self.children {
                Some(ref children) => {
                    // Interior node, use approximation of sub tree or recurse?
                    let s  = self.x2 - self.x1;
                    let dx = self.px - px;
                    let dy = self.py - py;
                    let d  = (dx * dx + dy * dy).sqrt();
                    if s / d < theta
                    {
                        // Approximate
                        let (fx_add, fy_add) = force(px, py, m, self.px, self.py, self.m);
                        fx = fx_add;
                        fy = fy_add;
                    } else {
                        // Recurse. Unsafe indexing is slightly faster than
                        // the obvious 'for child in children.into_iter() { ... }'
                        for i in 0..4 {
                            let (fx_add, fy_add) = unsafe {
                                children.get_unchecked(i).compute_force(px, py, m, theta)
                            };
                            fx += fx_add;
                            fy += fy_add;
                        }
                    }
                }
                None => {
                    // Skip our own entry in the tree (i /= j)
                    if self.px == px && self.py == py { return (0.0, 0.0) }

                    // Skip empty exterior nodes
                    if self.m == 0.0 { return (0.0, 0.0) }

                    // Compute force with particle in this exterior node
                    let (fx_add, fy_add) = force(px, py, m, self.px, self.py, self.m);
                    fx = fx_add;
                    fy = fy_add;
                }
            }
            (fx, fy)
        }
    }

    let mut particles_mtx = PARTICLES.lock().unwrap();

    // Build quad tree
    let mut tree;
    {
        let particles = &(* particles_mtx);

        // AABB of the whole simulation
        let mut x1 = f32::MAX;
        let mut y1 = f32::MAX;
        let mut x2 = f32::MIN;
        let mut y2 = f32::MIN;
        for p in & (* particles_mtx) {
            // No Ord trait for f32, hence no min/max
            x1 = if p.px < x1 { p.px } else { x1 };
            y1 = if p.py < y1 { p.py } else { y1 };
            x2 = if p.px > x2 { p.px } else { x2 };
            y2 = if p.py > y2 { p.py } else { y2 };
        }

        // TODO: Our quad tree is not necessarily quadratic, investigate whether
        //       that's a good thing
        //
        // if x2 - x1 > y2 - y1 {
        //     y2 = y1 + (x2 - x1)
        // } else {
        //     x2 = x1 + (y2 - y1)
        // }

        // Root node
        tree = Node::new(x1, y1, x2, y2);

        // Insert all particles into the tree
        for p in particles {
            tree.insert(p.px, p.py, p.m, 0);
        }
    }

    // Update particles
    {
        let particles = &mut (* particles_mtx);

        // Compute forces using the quad tree
        for p in particles {
            let (fx, fy) = tree.compute_force(p.px, p.py, p.m, theta);

            // Update velocity
            // F = ma, a = F / m
            p.vx += dt * fx / p.m;
            p.vy += dt * fy / p.m;

            // Update position
            p.px += dt * p.vx;
            p.py += dt * p.vy;
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

    let col_body = rgb_to_abgr32(255, 215, 130, 0.3);
    let col_tail = rgb_to_abgr32(255, 215, 130, 0.25);

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
                col = col_body;
            } else {
                // Tail
                let angle = p.vy.atan2(p.vx);
                let octant = ((8.0 * angle / (2.0 * consts::PI) + 8.0) as i32) % 8;
                let dir : [(i32, i32); 8] = [
                    ( 1,  0), // E
                    ( 1,  1), // NE
                    ( 0,  1), // N
                    (-1,  1), // NW
                    (-1,  0), // W
                    (-1, -1), // SW
                    ( 0, -1), // S
                    ( 1, -1)  // SE
                ];
                xo = x as i32 - dir[octant as usize].0;
                yo = y as i32 - dir[octant as usize].1;
                col = col_tail;
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

fn rgb_to_abgr32(r: u8, g: u8, b: u8, factor: f32) -> u32 {
    let r = (r as f32 * factor) as u32;
    let g = (g as f32 * factor) as u32;
    let b = (b as f32 * factor) as u32;
    (if r > 255 { 255 } else { r } << 0 ) |
    (if b > 255 { 255 } else { b } << 16) |
    (if g > 255 { 255 } else { g } << 8 )
}

fn add_abgr32(c1: u32, c2: u32) -> u32 {
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

    (ar << 24) | (br << 16) | (gr << 8) | (rr << 0)
}

