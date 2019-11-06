import "lib/github.com/diku-dk/lys/lys"

import "fastcast"

let test_spheres: []sphere =
  [ {center={x=0, y=0, z=0}, radius=0, color={r=0, g=0, b=0}} -- necessary dummy sphere
  , {center={x=100, y=100, z=240}, radius=120, color={r=0.9, g=0.1, b=0.8}}
  , {center={x=150, y=200, z=260}, radius=100, color={r=1.0, g=0.0, b=0.0}}
  , {center={x= -150, y= -200, z=160}, radius=100, color={r=0.1, g=0.2, b=0.9}}
  , {center={x=0, y=0, z=2300}, radius=1000, color={r=0.1, g=0.95, b=0.2}}
  ] ++ map (\i -> {center={x=200.0 + f32.tan i * 53.0, y= -200.0 + f32.sin i * 50.0, z=240.0 - i * 2.0},
                   radius=20.0 * f32.sqrt (f32.abs (f32.tan i)),
                   color={r=f32.abs (f32.tan i), g=0.8, b=f32.abs (f32.cos i)}}) (map r32 (0..<50))

let test_lights: []light =
  [ {position={x=300, y=200, z=50}, intensity=80000,
     color={r=0.5, g=0.5, b=0.5}} -- medium grey light
  , {position={x= -400, y= -100, z=150}, intensity=30000,
     color={r=0.8, g=0.1, b=0.1}} -- small reddish light
  , {position={x= -500, y= -200, z=850}, intensity=300000,
     color={r=0.0, g=0.15, b=0.9}} -- large blueish light
  ]

type keys_state = {shift: bool, down: bool, up: bool, left: bool, right: bool,
                   pagedown: bool, pageup: bool, minus: bool, plus: bool}

type text_content = (i32, i32, i32, f32, f32, f32, f32, f32, f32, f32)
module lys: lys with text_content = text_content = {
  type state = {h: i32, w: i32,
                screen_view_dest: f32, eye: eye,
                spheres: []sphere, lights: []light,
                keys: keys_state}
  type text_content = text_content

  let init _ (h: i32) (w: i32): state =
    {w, h,
     screen_view_dest=800, eye={position={x=0, y=0, z=0},
                                orientation={x=0, y=0, z=0}},
     spheres=copy test_spheres,
     lights=copy test_lights,
     keys={shift=false, down=false, up=false, left=false, right=false,
           pagedown=false, pageup=false, minus=false, plus=false}}

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h with w = w

  let keychange k pressed (keys: keys_state): keys_state =
    if k == SDLK_LSHIFT
    then keys with shift = pressed
    else if k == SDLK_RSHIFT
    then keys with shift = pressed
    else if k == SDLK_DOWN
    then keys with down = pressed
    else if k == SDLK_UP
    then keys with up = pressed
    else if k == SDLK_LEFT
    then keys with left = pressed
    else if k == SDLK_RIGHT
    then keys with right = pressed
    else if k == SDLK_PAGEDOWN
    then keys with pagedown = pressed
    else if k == SDLK_PAGEUP
    then keys with pageup = pressed
    else if k == SDLK_MINUS
    then keys with minus = pressed
    else if k == SDLK_PLUS
    then keys with plus = pressed
    else keys

  let step_eye (move_factor: f32) (keys: keys_state) (eye0: eye) =
    let move_eye op (eye : eye) =
      let point = eye.position with z = op eye.position.z (5 * move_factor)
      in eye with position = rotate_point eye.orientation eye.position point

    let turn_eye op (eye : eye) =
      eye with orientation.y = op eye.orientation.y (0.01 * move_factor)

    let elevate_eye op (eye : eye) =
      eye with position.y = op eye.position.y (5 * move_factor)

    let eye1 = if keys.down
               then move_eye (-) eye0
               else if keys.up
               then move_eye (+) eye0
               else eye0
    let eye2 = if keys.left
               then turn_eye (-) eye1
               else if keys.right
               then turn_eye (+) eye1
               else eye1
    let eye3 = if keys.pagedown
               then elevate_eye (+) eye2
               else if keys.pageup
               then elevate_eye (-) eye2
               else eye2
    in eye3

  let step td (s: state) =
    let move_factor = 60 * td * if s.keys.shift then 4 else 1
    let eye' = step_eye move_factor s.keys s.eye
    let screen_view_dest' = if s.keys.minus
                            then s.screen_view_dest + 5 * move_factor
                            else if s.keys.plus
                            then s.screen_view_dest - 5 * move_factor
                            else s.screen_view_dest
   -- XXX: We should be able to get rid of these copies.
   let spheres' = copy s.spheres
   let lights' = copy s.lights
   let spheres'[3] = spheres'[3] with center.x = spheres'[3].center.x + 0.3
   let lights'[0] = lights'[0] with position.x = lights'[0].position.x - 0.3
                               with position.y = lights'[0].position.y - 0.2
   in s with eye = eye'
        with screen_view_dest = screen_view_dest'
        with spheres = spheres'
        with lights = lights'

  let event (e: event) (s: state) =
    match e
    case #step td -> step td s
    case #wheel _ -> s
    case #mouse _ -> s
    case #keydown {key} ->  s with keys = keychange key true s.keys
    case #keyup {key} -> s with keys = keychange key false s.keys

  let render (s: state) =
    render s.w s.h s.screen_view_dest s.eye s.spheres s.lights

  let text_format = "FPS: %d\nSpheres: %d; lights: %d\nPosition: (%.2f, %.2f, %.2f)\nOrientation: (%.2f, %.2f, %.2f)\nView dist.: %.2f"

  let text_content (fps: f32) (s: state): text_content =
    (t32 fps,
     length s.spheres, length s.lights,
     s.eye.position.x, s.eye.position.y, s.eye.position.z,
     s.eye.orientation.x, s.eye.orientation.y, s.eye.orientation.z,
     s.screen_view_dest)

  let text_colour = const argb.white

  let grab_mouse = false
}
