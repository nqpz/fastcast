import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32

type position = vec3.vector
type position_maybe = (position, bool)

type direction = vec3.vector
type angle = vec3.vector

type rgb = {r: f32, g: f32, b: f32}
type argb = i32

type sphere = {center: position,
               radius: f32,
               color: rgb}

type light = {position: position,
              intensity: f32,
              color: rgb}

type line = {origin: position,
             direction: direction}

type eye = {position: position,
            orientation: angle}

-- Rotate a point around an origo.  We only really use a small part of this, but
-- here it is.
let rotate_point (angle: angle) (origo: position) (point: position): position =
  let (x0, y0, z0) = (point.x - origo.x, point.y - origo.y, point.z - origo.z)

  let (sin_x, cos_x) = (f32.sin angle.x, f32.cos angle.x)
  let (sin_y, cos_y) = (f32.sin angle.y, f32.cos angle.y)
  let (sin_z, cos_z) = (f32.sin angle.z, f32.cos angle.z)

  -- X axis.
  let (x1, y1, z1) = (x0,
                      y0 * cos_x - z0 * sin_x,
                      y0 * sin_x + z0 * cos_x)
  -- Y axis.
  let (x2, y2, z2) = (z1 * sin_y + x1 * cos_y,
                      y1,
                      z1 * cos_y - x1 * sin_y)
  -- Z axis.
  let (x3, y3, z3) = (x2 * cos_z - y2 * sin_z,
                      x2 * sin_z + y2 * cos_z,
                      z2)

  let (x', y', z') = (origo.x + x3, origo.y + y3, origo.z + z3)
  in {x=x', y=y', z=z'}

-- Find intersections.  A sphere and a line has at most two intersections.  Note
-- that one or both of the returned positions have NaN values if there are not
-- two intersections.
local let ray_sphere_intersections (ray: line) (sphere: sphere): (position, position) =
  let diff = ray.origin vec3.- sphere.center
  let core = vec3.dot ray.direction diff
  let det = core ** 2.0 - (vec3.norm diff) ** 2.0 + sphere.radius ** 2.0
  let find_hit (d: f32): position = ray.origin vec3.+ (vec3.scale d ray.direction)
  let s = f32.sqrt det
  let d0 = s - core
  let d1 = -s - core
  in (find_hit d0, find_hit d1)

-- Encode the distance and sphere index into a 64-bit number for use in finding
-- the sphere with the smallest distance to the eye.  'dot' is either -1 or +1,
-- and we use it to determine if the object is in front of us (good) or behind
-- us (ignore).  'dist' can be NaN, which we also handle.
local let encode_dist_and_index (dist: f32) (dot: f32) (i: i32): i64 =
  let k0 = i32.f32 (dot / 2.0 + 1.0)
  let k1 = i32.f32 (f32.abs dist + 1.5)
  let k1 = k1 // k1
  let k = k0 * k1 in
  (i64.i32 (k * i32.f32 dist + (1 - k) * i32.highest) << 32) | i64.i32 (k * i)

-- Extract the index, which is stored on the 32 least significant bits.
local let decode_index (code: i64): i32 =
  i32.i64 code

-- Create rays for casting.
local let make_ray (screen_view_dist: f32) (eye: eye) (origin: position): line =
  let origin_origo = {x=0, y=0, z= -screen_view_dist}
  let origin' = rotate_point eye.orientation origin_origo origin

  let slope_x = origin.x / screen_view_dist
  let slope_y = origin.y / screen_view_dist
  let direction = {x=slope_x, y=slope_y, z=1.0}
  let direction_origo = {x=0, y=0, z=0}
  let direction' = rotate_point eye.orientation direction_origo direction

  let direction'' = vec3.normalise direction'
  let origin'' = origin' vec3.+ eye.position
  in {origin=origin'', direction=direction''}

local let make_rays (width: i32) (height: i32) (screen_view_dist: f32) (eye: eye): [][]line =
  let make_ray_from_screen ((x, y): (i32, i32)): line =
    let origin = {x=r32 x - r32 width / 2.0,
                  y=r32 y - r32 height / 2.0,
                  z=0}
    in make_ray screen_view_dist eye origin
  in map (map make_ray_from_screen)
         (tabulate_2d height width (\y x -> (x, y)))

-- Find the closest intersection between the ray and a sphere, and return both
-- the hit position and the sphere.  If there is no hit, the position will be
-- all NaN values, and the sphere will be a dummy sphere with radius 0.
local let find_intersection_hit [n_spheres]
 (screen_view_dist: f32)
 (eye: eye)
 (ray: line)
 (spheres: [n_spheres]sphere): (position, sphere) =
  let prep (i: i32) (sphere: sphere): i64 =
    let (p0, p1) = ray_sphere_intersections ray sphere
    let middle_ray = make_ray screen_view_dist eye {x=0, y=0, z=0}
    let backwards_bonus =
      ray.origin vec3.- vec3.scale (screen_view_dist
                                    / vec3.dot middle_ray.direction ray.direction)
                                   ray.direction
    let dot0 = vec3.dot ray.direction (vec3.normalise (p0 vec3.- backwards_bonus))
    let dot1 = vec3.dot ray.direction (vec3.normalise (p1 vec3.- backwards_bonus))
    let (dist0, dist1) = (vec3.norm (p0 vec3.- backwards_bonus),
                          vec3.norm (p1 vec3.- backwards_bonus))
    let (i0, i1) = (i * 2,
                    i * 2 + 1)

    in i64.min (encode_dist_and_index dist0 dot0 i0)
               (encode_dist_and_index dist1 dot1 i1)

  let ne = i64.i32 i32.highest << 32
  let res = reduce_comm (\s0 s1 -> i64.min s0 s1) ne
                        (map2 prep (0..<n_spheres) spheres)

  let i0 = decode_index res
  let s = unsafe spheres[i0 // 2]
  let (p0, p1) = ray_sphere_intersections ray s
  let k = f32.i32 (i0 & 1)
  let p = (vec3.scale (1.0 - k) p0) vec3.+ (vec3.scale k p1)
  in (p, s)

local let clamp_rgb (part: i32): i32 = i32.max 0 (i32.min 255 part)
local let clamp_f32 (color: f32): f32 = f32.max 0.0 (f32.min 1.0 color)

let render (width: i32) (height: i32)
           (screen_view_dist: f32) (eye: eye)
           (spheres: []sphere) (lights: []light): [][]argb =
  let rays = make_rays width height screen_view_dist eye
  let find_color (ray: line): argb =
    let (hit, sphere) = find_intersection_hit screen_view_dist eye ray spheres
    let normal = vec3.normalise (hit vec3.- sphere.center)
    let light_mag (light: light) =
      let v = light.position vec3.- hit
      let dist = vec3.norm v
      let dir = vec3.scale (1.0 / dist) v
      let mag = clamp_f32 (light.intensity * vec3.dot normal dir / (dist * dist))
      in mag

    -- Find the final color.  This might be somewhat wrong.
    let mags = map light_mag lights
    let mag = clamp_f32 (f32.sum mags)
    let light_r = clamp_f32 (f32.sum (map2 (\l m -> l.color.r * m) lights mags) / f32.sum mags)
    let light_g = clamp_f32 (f32.sum (map2 (\l m -> l.color.g * m) lights mags) / f32.sum mags)
    let light_b = clamp_f32 (f32.sum (map2 (\l m -> l.color.b * m) lights mags) / f32.sum mags)
    let light_color = {r=light_r, g=light_g, b=light_b}
    let ambient = 0.2
    let convert s l = clamp_rgb (i32.f32 ((ambient * s + (1.0 - ambient) * mag * l) * 255.0))
    in 255 << 24
       | convert sphere.color.r light_color.r << 16
       | convert sphere.color.g light_color.g << 8
       | convert sphere.color.b light_color.b
  in map (map find_color) rays
