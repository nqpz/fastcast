import "/futlib/math"
import "/futlib/colour"
import "/futlib/vec3"

module vec3 = mk_vec3 f32

type position = vec3.vec
type position_maybe = (position, bool)

type direction = vec3.vec

type sphere = {center: position,
               radius: f32}

type line = {origin: position,
             direction: direction}

type eye = {position: position,
            orientation: direction}

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

let find_hit [n] (ray: line) (spheres: [n]sphere): (position, i32, position) =

  let stuff i =
    let (p0, p1) = cast_ray_sphere ray (unsafe spheres[i])
    let (i0, i1) = (i * 2,
                    i * 2 + 1)
    in i64.min (merge p0.z i0) (merge p1.z i1)

  let res = foldl (\res i -> i64.min res (stuff i)) (stuff 0) (1..<n)

  let s = unsafe spheres[unmerge res // 2]
  let (p0, p1) = cast_ray_sphere ray s
  let p = unsafe ([p0, p1])[unmerge res & 1]
  let r = i32.f32 ((p.z + 0.5) / p.z) * 60
  in (p, r, s.center)

let make_rays (width: i32) (height: i32) (screen_view_dist: f32) (eye: eye): [width][height]line =
  let make_ray_from_screen (x: i32) (y: i32): line =
    let origin = {x=r32 x - r32 width / 2.0,
                  y=r32 y - r32 height / 2.0,
                  z=eye.position.z}
    let slope_x = origin.x / screen_view_dist
    let slope_y = origin.y / screen_view_dist
    let direction = {x=slope_x, y=slope_y, z=1.0}
    let direction' = vec3.normalise direction
    let origin' = origin vec3.+ {x=eye.position.x, y=eye.position.y, z=0.0}
    in {origin=origin', direction=direction'}
  in map (\x -> map (\y -> make_ray_from_screen x y) (0..<height)) (0..<width)

let light: position = {x= 200, y=100, z= 50}

let render (width: i32) (height: i32) (screen_view_dist: f32) (eye: eye) (spheres: []sphere): [width][height]argb.colour =
  let rayss = make_rays width height screen_view_dist eye
  let find_colour (ray: line): argb.colour =
    let (h, k, center) = find_hit ray spheres
    let normal = vec3.normalise (h vec3.- center)
    let v = light vec3.- h
    let dist = vec3.norm v
    let dir = vec3.scale (1.0 / dist) v
    let intensity = 100000.0
    let mag = intensity * vec3.dot normal dir / (dist * dist)
    in 255 << 24 | i32.max k (i32.min 255 (i32.f32 (mag * 255.0)))
  in map (\rays -> map find_colour rays) rayss

let main [n]
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
 : [width][height]argb.colour =
  let eye = {position={x=eye_position_x, y=eye_position_y, z=eye_position_z},
             orientation={x=eye_orientation_x, y=eye_orientation_y, z=eye_orientation_z}}
  let spheres = map4 (\x y z r -> {center={x=x, y=y, z=z}, radius=r})
                sphere_center_xs sphere_center_ys sphere_center_zs sphere_radiuses
  in render width height screen_view_dist eye spheres
