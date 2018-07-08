import "/futlib/math"
import "/futlib/colour"
import "/futlib/vec3"

module vec3 = mk_vec3 f32

type position = vec3.vec
type position_maybe = (position, bool)

type direction = vec3.vec
type angle = vec3.vec

type rgb = {r: f32, g: f32, b: f32}

type sphere = {center: position,
               radius: f32,
               color: rgb}

type light = {position: position,
              intensity: f32}

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
  let d0 = -core + s
  let d1 = -core - s
  in (find_hit d0, find_hit d1)

let merge (z: f32) (i: i32): i64 =
  (((i64.f32 z) + i64.i32 (i32.largest * (1 - i32.f32 ((z + 0.5) / z)))) << 32) | i64.i32 i

let unmerge (t: i64): i32 =
  i32.i64 t

let find_hit [n] (ray: line) (spheres: [n]sphere): (position, i32, sphere) =

  let stuff i =
    let (p0, p1) = cast_ray_sphere ray (unsafe spheres[i])
    let (i0, i1) = (i * 2,
                    i * 2 + 1)
    in i64.min (merge p0.z i0) (merge p1.z i1)

  -- let res = foldl (\res i -> i64.min res (stuff i)) (stuff 0) (1..<n)

  let res = reduce_comm (\s0 s1 -> i64.min s0 s1) (stuff 0) (map stuff (1..<n))

  let s = unsafe spheres[unmerge res // 2]
  let (p0, p1) = cast_ray_sphere ray s
  -- let p = unsafe ([p0, p1])[unmerge res & 1]
  --  let p = if unmerge res & 1 == 0 then p0 else p1
  let k = f32.i32 (unmerge res & 1)
  let p = (vec3.scale (1.0 - k) p0) vec3.+ (vec3.scale k p1)
  let mask = i32.f32 ((p.z + 0.5) / p.z)
  in (p, mask, s)

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

let make_rays (width: i32) (height: i32) (screen_view_dist: f32) (eye: eye): [width][height]line =
  let make_ray_from_screen (x: i32) (y: i32): line =
    let origin = {x=r32 x - r32 width / 2.0,
                  y=r32 y - r32 height / 2.0,
                  z=0}
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
  in map (\x -> map (\y -> make_ray_from_screen x y) (0..<height)) (0..<width)

let render
 (width: i32) (height: i32)
 (screen_view_dist: f32) (eye: eye)
 (spheres: []sphere) (lights: []light)
 : [width][height]argb.colour =
  let rayss = make_rays width height screen_view_dist eye
  let find_colour (ray: line): argb.colour =
    let (h, mask, sphere) = find_hit ray spheres
    let normal = vec3.normalise (h vec3.- sphere.center)
    let light_mag (light: light) =
      let v = light.position vec3.- h
      let dist = vec3.norm v
      let dir = vec3.scale (1.0 / dist) v
      let mag = light.intensity * vec3.dot normal dir / (dist * dist)
      in mag
    let mag = f32.min 1.0 (f32.sum (map light_mag lights))

    let ambient = 0.24
    let convert t = i32.max 0 (i32.min 255 (i32.f32 ((f32.max (t * ambient * f32.i32 mask) mag) * t * 255.0)))

    in 255 << 24 | convert sphere.color.r << 16 | convert sphere.color.g << 8 | convert sphere.color.b
  in map (\rays -> map find_colour rays) rayss

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
 : [width][height]argb.colour =
  let eye = {position={x=eye_position_x, y=eye_position_y, z=eye_position_z},
             orientation={x=eye_orientation_x, y=eye_orientation_y, z=eye_orientation_z}}

  let spheres = map (\(x, y, z, radius, r, g, b) -> {center={x=x, y=y, z=z},
                                                     radius=radius,
                                                     color={r=r, g=g, b=b}})
                (zip
                 sphere_center_xs sphere_center_ys sphere_center_zs
                 sphere_radiuses
                 sphere_color_rs sphere_color_gs sphere_color_bs)

  let lights = map (\(x, y, z, i) -> {position={x=x, y=y, z=z},
                                      intensity=i})
               (zip
                light_position_xs
                light_position_ys
                light_position_zs
                light_intensities)

  in render width height screen_view_dist eye spheres lights
