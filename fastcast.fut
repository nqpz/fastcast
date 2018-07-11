import "/futlib/vector"

module vec3 = mk_vec3 f32

type position = vec3.vec
type position_maybe = (position, bool)

type direction = vec3.vec
type angle = vec3.vec

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

let cast_ray_sphere (ray: line) (sphere: sphere): (position, position) =
  let diff = ray.origin vec3.- sphere.center
  let core = vec3.dot ray.direction diff
  let det = core ** 2.0 - (vec3.norm diff) ** 2.0 + sphere.radius ** 2.0
  let find_hit (d: f32): position = ray.origin vec3.+ (vec3.scale d ray.direction)
  let s = f32.sqrt det
  let d0 = s - core
  let d1 = -s - core
  in (find_hit d0, find_hit d1)

let encode_dist_and_index (dist: f32) (dot: f32) (i: i32): i64 =
  let k = i32.f32 (dot / 2.0 + 1.0)
  let l = i32.f32 (f32.abs dist + 1.5)
  let l = l // l
  let k = k * l in
  if false --dot < 0.0 || f32.isnan dist
  then i64.i32 i32.largest << 32
  else (i64.i32 (k * i32.f32 dist + (1 - k) * i32.largest) << 32) | i64.i32 (k * i)
--  (i64.i32 (i32.f32 dist + i32.largest) << 32) | i64.i32 i
--  let t = i64.i32 i32.largest --(i32.largest * (1 - i32.f32 ((z + 0.5) / z)))
  --  in ((i64.f32 dist + t) << 32) | i64.i32 i
--  in ((i64.f32 dist + t) << 32) | i64.i32 i
--  in ((u64.u32 (u32.f32 dist)) << 32) | u64.i32 i
--  in ((i64.f32 z + t) << 32) | i64.i32 i
--  in ((i64.i32 (i32.f32 z)) << 32) | i64.i32 i
--  in ((u64.f32 z + t) << 32) | u64.i32 i
--  in ((u64.f32 z) << 32) | u64.i32 i
--  in ((u64.u32 (u32.f32 z)) << 32) | u64.i32 i

let decode_index (code: i64): i32 =
  i32.i64 code

let rotate_point (angle: vec3.vec) (origo: vec3.vec) (point: vec3.vec): vec3.vec =
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

let make_ray (screen_view_dist: f32) (eye: eye) (origin: position): line =
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

let make_rays (width: i32) (height: i32) (screen_view_dist: f32) (eye: eye): []line =
  let make_ray_from_screen ((x, y): (i32, i32)): line =
    let origin = {x=r32 x - r32 width / 2.0,
                  y=r32 y - r32 height / 2.0,
                  z=0}
    in make_ray screen_view_dist eye origin
  in map make_ray_from_screen (flatten (map (\x -> map (\y -> (x, y)) (0..<height)) (0..<width)))

let find_hit [n] (screen_view_dist: f32) (eye: eye) (origin: position) (ray: line) (spheres: [n]sphere): (position, i32, sphere) =

  let stuff (i: i32) (sphere: sphere): i64 =
    let sphere = sphere
    let base_ray = make_ray screen_view_dist eye {x=0, y=0, z=0}
    let tt = vec3.scale (screen_view_dist / vec3.dot base_ray.direction ray.direction) ray.direction
    let (p0, p1) = cast_ray_sphere ray sphere
    let b0 = vec3.dot ray.direction (vec3.normalise (p0 vec3.- (ray.origin vec3.- tt)))
    let b1 = vec3.dot ray.direction (vec3.normalise (p1 vec3.- (ray.origin vec3.- tt)))
    let (dist0, dist1) = (b0 * vec3.norm (p0 vec3.- (ray.origin vec3.- tt)),
                          b1 * vec3.norm (p1 vec3.- (ray.origin vec3.- tt)))
    let (i0, i1) = (i * 2,
                    i * 2 + 1)

    in i64.min (encode_dist_and_index dist0 b0 i0) (encode_dist_and_index dist1 b1 i1)

  let s_base = i64.i32 i32.largest << 32 --stuff 0 (unsafe spheres[0])
  let res = reduce_comm (\s0 s1 -> i64.min s0 s1) s_base (map2 stuff (0..<n) spheres)

  let i0 = decode_index res
  let s = unsafe spheres[i0 // 2]
  let (p0, p1) = cast_ray_sphere ray s
  let k = f32.i32 (i0 & 1)
  let p = (vec3.scale (1.0 - k) p0) vec3.+ (vec3.scale k p1)
  let mask = i32.f32 ((p.z + 0.5) / p.z)
  in (p, mask, s)

let clamp_rgb (part: i32): i32 = i32.max 0 (i32.min 255 part)

let clamp_argb (color: f32): f32 = f32.max 0.0 (f32.min 1.0 color)

--let f32_to_i32_color (color

let render [n_lights]
 (width: i32) (height: i32)
 (screen_view_dist: f32) (eye: eye)
 (spheres: []sphere) (lights: [n_lights]light)
 : []argb =
  let rays = make_rays width height screen_view_dist eye
  let find_colour (ray: line): argb =
    let (h, mask, sphere) = find_hit screen_view_dist eye ray.origin ray spheres
    let normal = vec3.normalise (h vec3.- sphere.center)
    let light_mag (light: light) =
      let v = light.position vec3.- h
      let dist = vec3.norm v
      let dir = vec3.scale (1.0 / dist) v
      let mag = clamp_argb (light.intensity * vec3.dot normal dir / (dist * dist))
      in mag
    let mags = map light_mag lights
    let mag = clamp_argb (f32.sum mags)
    let light_r = clamp_argb (f32.sum (map2 (\l m -> l.color.r * m) lights mags) / f32.sum mags)
    let light_g = clamp_argb (f32.sum (map2 (\l m -> l.color.g * m) lights mags) / f32.sum mags)
    let light_b = clamp_argb (f32.sum (map2 (\l m -> l.color.b * m) lights mags) / f32.sum mags)
    let light_color = {r=light_r, g=light_g, b=light_b}
    let ambient = 0.2
--    let convert t = clamp_rgb (i32.f32 ((f32.max (t * ambient * f32.i32 mask) mag) * t * 255.0))
    let convert t l = clamp_rgb (i32.f32 ((ambient * t + (1.0 - ambient) * mag * l) * 255.0))
    in 255 << 24 | convert sphere.color.r light_color.r << 16 | convert sphere.color.g light_color.g << 8 | convert sphere.color.b light_color.b
  in map find_colour rays

let main [m] [n]
 (width: i32)
 (height: i32)
 (screen_view_dist: f32)
 (eye_position_x: f32)
 (eye_position_y: f32)
 (eye_position_z: f32)
 (eye_orientation_x: f32)
 (eye_orientation_y: f32)
 (eye_orientation_z: f32)
 (sphere_center_xs: [n]f32)
 (sphere_center_ys: [n]f32)
 (sphere_center_zs: [n]f32)
 (sphere_radiuses: [n]f32)
 (sphere_color_rs: [n]f32)
 (sphere_color_gs: [n]f32)
 (sphere_color_bs: [n]f32)
 (light_position_xs: [m]f32)
 (light_position_ys: [m]f32)
 (light_position_zs: [m]f32)
 (light_intensities: [m]f32)
 (light_color_rs: [m]f32)
 (light_color_gs: [m]f32)
 (light_color_bs: [m]f32)
 : []argb =
  let eye = {position={x=eye_position_x, y=eye_position_y, z=eye_position_z},
             orientation={x=eye_orientation_x, y=eye_orientation_y, z=eye_orientation_z}}

  let spheres: [n]sphere = map (\(x, y, z, radius, r, g, b) ->
                                {center={x=x, y=y, z=z},
                                 radius=radius,
                                 color={r=r, g=g, b=b}})
    (zip7
     sphere_center_xs sphere_center_ys sphere_center_zs
     sphere_radiuses
     sphere_color_rs sphere_color_gs sphere_color_bs)

  let lights: [m]light = map (\(x, y, z, i, r, g, b) ->
                              {position={x=x, y=y, z=z},
                               intensity=i,
                               color={r=r, g=g, b=b}})
    (zip7
     light_position_xs light_position_ys light_position_zs
     light_intensities
     light_color_rs light_color_gs light_color_bs)

  in render width height screen_view_dist eye spheres lights
