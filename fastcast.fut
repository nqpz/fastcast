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
            direction: direction}

--let no_position: position_maybe = ({x=0, y=0, z=10000}, false)
--let no_position: position = {x=0, y=0, z=10000}

--let cast_ray_sphere (ray: line) (sphere: sphere): (position_maybe, position_maybe) =
let cast_ray_sphere (ray: line) (sphere: sphere): (position, position) =  
  let diff = ray.origin vec3.- sphere.center
  let core = vec3.dot ray.direction diff
  let det = core ** 2.0 - (vec3.norm diff) ** 2.0 + sphere.radius ** 2.0
  let find_hit (d: f32): position = ray.origin vec3.+ (vec3.scale d ray.direction)
  -- 'det' negative?
  let s = f32.sqrt det
          let d0 = -core + s
          let d1 = -core - s
--          in ((find_hit d0, true), (find_hit d1, true))
          in (find_hit d0, find_hit d1)
  -- in if det < 0.0
  --    then (no_position, no_position)
  --    else if det == 0.0
  --    then let d = core
  --         in ((find_hit d, true), no_position)
  --    else 

--let cast_ray (ray: line) (spheres: []sphere): []position_maybe =
--  let (xs, ys) = unzip (map (cast_ray_sphere ray) spheres)
--  in concat xs ys
--  flatten (map (\sphere -> let (p0, p1) = cast_ray_sphere ray sphere in [p0, p1]) spheres)

  --let pick ((h0, ok0): position_maybe) ((h1, ok1): position_maybe): position_maybe =
-- let pick (h0: position) (h1: position): position =  
--   let t = h1.z < h0.z -- !ok0 || h1.z < h0.z
--   let h = if t
--           then h1
--           else h0
-- --  let ok = true -- && ok1 || ok0
-- --  in (h, ok)
--   in h

--let find_front_position [n] (hits: [n]position_maybe): position_maybe =
--  loop res = no_position for i < n do pick res hits[i]
--  reduce_comm pick no_position hits

let merge (z: f32) (i: i32): i64 =
  (((i64.f32 z) + i64.i32 (10000 * (1 - i32.f32 ((z + 0.5) / z)))) << 32) | i64.i32 i

let unmerge (t: i64): i32 =
  i32.i64 t

let find_hit_maybe [n] (ray: line) (spheres: [n]sphere): (position, i32, position) =

  let stuff i =
    let (p0, p1) = cast_ray_sphere ray spheres[i]
    let (i0, i1) = (i * 2,
                    i * 2 + 1)
    in i64.min (merge p0.z i0) (merge p1.z i1)
--  let ne = merge (10000.0) 0

  let res = foldl (\res i -> i64.min res (stuff i)) (stuff 0) (1..<n)
  -- let res = loop res = stuff 0 for i < n - 1 do
  --   i64.min res (stuff (i + 1))
  -- let res = loop res = ne for i < n do
  --   stuff res i


  
  let s = unsafe spheres[unmerge res // 2]
  let (p0, p1) = cast_ray_sphere ray s
  let p = unsafe ([p0, p1])[unmerge res & 1]
--  let r0 = i32.f32 p.z

--  let r = if f32.isnan p.z then 0 else 60
--  let r = if i32.f32 p.z == 0 then 0 else 40
  let r = i32.f32 ((p.z + 0.5) / p.z) * 60
--  let r = 0
--  let r = r0 // r0
  in (p, r, s.center)

  -- let (vals, res) =
  --   loop (vals, res) = (replicate m no_position, merge no_position.z 0) for i < n do
  --   let (p0, p1) = cast_ray_sphere ray spheres[i]
  --   let (i0, i1) = (1 + i * 2, 1 + i * 2 + 1)
  --   let vals' = (vals with [i0] <- p0) with [i1] <- p1
  --   in (vals', i64.min res (i64.min (merge p0.z i0) (merge p1.z i1)))
  -- in unsafe vals[unmerge res]


--  let ts = map2 (\v i -> merge v.z i) vals (iota m)
--  let t = reduce_comm i64.min (merge no_position.z 0) ts
--  let t = loop res = merge no_position.z 0 for i < m do
--          i64.min res (merge vals[i].z i) --ts[i]
--  in unsafe vals[unmerge t]

--      in i64.min res (i64.min (merge p0.z ((i + 1) * 2)) (merge p1.z ((i + 1) * 2 + 1)))
--  let l = unmerge k
--  let (p0, p1) = cast_ray_sphere ray spheres[l // 2]
  

--  (cast_ray_sphere ray spheres[0]).1
  -- loop res = no_position for i < n // 16 do

  -- let (p00, p01) = cast_ray_sphere ray spheres[i * 16 + 00]
  -- let p01t = pick p00 p01
  -- let (p02, p03) = cast_ray_sphere ray spheres[i * 16 + 01]
  -- let p03t = pick p02 p03
  -- let p03tu = pick p01t p03t
  -- let (p04, p05) = cast_ray_sphere ray spheres[i * 16 + 02]
  -- let p05t = pick p04 p05
  -- let (p06, p07) = cast_ray_sphere ray spheres[i * 16 + 03]
  -- let p07t = pick p06 p07
  -- let p07tu = pick p05t p07t
  -- let p07tuv = pick p03tu p07tu
  -- let (p08, p09) = cast_ray_sphere ray spheres[i * 16 + 04]
  -- let p09t = pick p08 p09
  -- let (p10, p11) = cast_ray_sphere ray spheres[i * 16 + 05]
  -- let p11t = pick p10 p11
  -- let p11tu = pick p09t p11t
  -- let (p12, p13) = cast_ray_sphere ray spheres[i * 16 + 06]
  -- let p13t = pick p12 p13
  -- let (p14, p15) = cast_ray_sphere ray spheres[i * 16 + 07]
  -- let p15t = pick p14 p15
  -- let p15tu = pick p13t p15t
  -- let p15tuv = pick p11tu p15tu
  -- let p15tuvw = pick p07tuv p15tuv
  -- let (p16, p17) = cast_ray_sphere ray spheres[i * 16 + 08]
  -- let p17t = pick p16 p17
  -- let (p18, p19) = cast_ray_sphere ray spheres[i * 16 + 09]
  -- let p19t = pick p18 p19
  -- let p19tu = pick p17t p19t
  -- let (p20, p21) = cast_ray_sphere ray spheres[i * 16 + 10]
  -- let p21t = pick p20 p21
  -- let (p22, p23) = cast_ray_sphere ray spheres[i * 16 + 11]
  -- let p23t = pick p22 p23
  -- let p23tu = pick p21t p23t
  -- let p23tuv = pick p19tu p23tu
  -- let (p24, p25) = cast_ray_sphere ray spheres[i * 16 + 12]
  -- let p25t = pick p24 p25
  -- let (p26, p27) = cast_ray_sphere ray spheres[i * 16 + 13]
  -- let p27t = pick p26 p27
  -- let p27tu = pick p25t p27t
  -- let (p28, p29) = cast_ray_sphere ray spheres[i * 16 + 14]
  -- let p29t = pick p28 p29
  -- let (p30, p31) = cast_ray_sphere ray spheres[i * 16 + 15]
  -- let p31t = pick p30 p31
  -- let p31tu = pick p29t p31t
  -- let p31tuv = pick p27tu p31tu
  -- let p31tuvw = pick p23tuv p31tuv
  
  -- let p = pick p15tuvw p31tuvw
  -- in pick res p

let screen_dist_z: f32 = 700.0

let make_rays (width: i32) (height: i32) (eye: eye): [width][height]line =
  let make_ray_from_screen (x: i32) (y: i32): line =
    let origin = {x=r32 x - r32 width / 2.0,
                  y=r32 y - r32 height / 2.0,
                  z=eye.position.z}
    let slope_x = origin.x / screen_dist_z
    let slope_y = origin.y / screen_dist_z
    let direction = {x=slope_x, y=slope_y, z=1.0}
    let direction' = vec3.normalise direction
    let origin' = origin vec3.+ {x=eye.position.x, y=eye.position.y, z=0.0}
    in {origin=origin', direction=direction'}
  in map (\x -> map (\y -> make_ray_from_screen x y) (0..<height)) (0..<width)

let light: position = {x= 200, y=100, z= 50}

let render (width: i32) (height: i32) (eye: eye) (spheres: []sphere): [width][height]argb.colour =
  let rayss = make_rays width height eye
  let find_colour (ray: line): argb.colour =
--    let (h, ok) = find_front_position (cast_ray ray spheres)
    let (h, k, center) = find_hit_maybe ray spheres
    -- in if ok
    --    then let normal = vec3.normalise (h vec3.- {x=30, y=30, z=100})
    --         let v = light vec3.- h
    --         let dist = vec3.norm v
    --         let dir = vec3.scale (1.0 / dist) v
    --         let mag = vec3.dot normal dir / (dist * dist)
    --         in argb.scale argb.magenta (-200000 * mag)
    --    else argb.black
    let normal = vec3.normalise (h vec3.- center)
    let v = light vec3.- h
    let dist = vec3.norm v
    let dir = vec3.scale (1.0 / dist) v
    let intensity = 100000.0
    let mag = intensity * vec3.dot normal dir / (dist * dist)
    --    in argb.magenta
    -- 255 min
    in 255 << 24 | i32.max k (i32.min 255 (i32.f32 (mag * 255.0)))
--    in argb.scale argb.magenta (-200000 * mag) -- r32 (if ok then 1 else 0))
  in map (\rays -> map find_colour rays) rayss

let test_spheres: []sphere =
  [ 
  {center={x= 100, y= 100, z=240},
   radius=120.0}  ,
    {center={x= 150, y= 200, z=240},
   radius=120.0}

    -- {center={x= 0, y= 100, z=180},
    --  radius=160.0}
    
    -- {center={x= -550, y=430, z=97},
    --  radius=90.0}  
  ]

let test_spheres1 (a: i32) (b: i32): []sphere =
  map (\i -> {center={x=r32 i * 20, y=r32 i * 15, z= 15 - f32.abs (r32 i) * 20}, radius=20.0}) (a...b)

let main = render 800 600 {position={x=0, y=0, z=0}, direction={x=0, y=0, z=0}} test_spheres --(test_spheres1 (-10) 10)
