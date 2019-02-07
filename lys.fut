import "lib/github.com/diku-dk/lys/lys"

module fastcast = import "fastcast"

let test_spheres: []fastcast.sphere =
  [ {center={x=0, y=0, z=0}, radius=0, color={r=0, g=0, b=0}} -- dummy sphere
  , {center={x=100, y=100, z=240}, radius=120, color={r=0.9, g=0.1, b=0.8}}
  , {center={x=150, y=200, z=260}, radius=100, color={r=1.0, g=0.0, b=0.0}}
  , {center={x= -150, y= -200, z=160}, radius=100, color={r=0.1, g=0.2, b=0.9}}
  , {center={x=0, y=0, z=2300}, radius=1000, color={r=0.1, g=0.95, b=0.2}}
  ] ++ map (\i -> {center={x=200.0 + f32.tan i * 53.0, y= -200.0 + f32.sin i * 50.0, z=240.0 - i * 2.0},
                   radius=20.0 * f32.sqrt (f32.abs (f32.tan i)),
                   color={r=f32.abs (f32.tan i), g=0.8, b=f32.abs (f32.cos i)}}) (map r32 (0..<50))

let test_lights: []fastcast.light =
  [ {position={x=300, y=200, z=50}, intensity=80000,
     color={r=0.5, g=0.5, b=0.5}} -- medium grey light
  , {position={x= -400, y= -100, z=150}, intensity=30000,
     color={r=0.8, g=0.1, b=0.1}} -- small reddish light
  , {position={x= -500, y= -200, z=850}, intensity=300000,
     color={r=0.0, g=0.15, b=0.9}} -- large blueish light
  ]

type keys_state = {shift: bool, down: bool, up: bool, left: bool, right: bool,
                   pagedown: bool, pageup: bool, minus: bool, plus: bool}

module lys: lys = {
  type state = {h: i32, w: i32,
                screen_view_dest: f32, eye: fastcast.eye,
                spheres: []fastcast.sphere, lights: []fastcast.light,
                keys: keys_state, show_stats: bool}

  let init (h: i32) (w: i32): state =
    {w, h,
     screen_view_dest=800, eye={position={x=0, y=0, z=0},
                                orientation={x=0, y=0, z=0}},
     spheres=copy test_spheres,
     lights=copy test_lights,
     keys={shift=false, down=false, up=false, left=false, right=false,
           pagedown=false, pageup=false, minus=false, plus=false},
     show_stats=true}

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

  let key (e: key_event) k (s: state) =
    let pressed = match e
                  case #keydown -> true
                  case #keyup -> false
    in if k == SDLK_h && pressed
       then s with show_stats = !s.show_stats
       else s with keys = keychange k pressed s.keys

  let step_eye (move_factor: f32) (keys: keys_state) (eye0: fastcast.eye) =
    let move_eye op (eye : fastcast.eye) =
      let point = eye.position with z = op eye.position.z (5 * move_factor)
      in eye with position = fastcast.rotate_point eye.orientation eye.position point

    let turn_eye op (eye : fastcast.eye) =
      eye with orientation.y = op eye.orientation.y (0.01 * move_factor)

    let elevate_eye op (eye : fastcast.eye) =
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
    in s with eye = eye' with screen_view_dest = screen_view_dest'

  let render (s: state) =
    fastcast.render s.w s.h s.screen_view_dest s.eye s.spheres s.lights

  let text (render_duration: f32) (s: state): []printf_input =
    let top = (spad "(Press h to hide/show stats)",
               [ printf_placeholder
               , printf_placeholder
               , printf_placeholder
               ], argb.white)
    in if !s.show_stats
       then [top]
       else [ top
            , (spad "Futhark render: %s ms",
               [ (#f32, printf_val with f32 = render_duration)
               , printf_placeholder
               , printf_placeholder
               ], argb.white)
            , (spad "Spheres: %s; lights: %s",
               [ (#i32, printf_val with i32 = length s.spheres)
               , (#i32, printf_val with i32 = length s.lights)
               , printf_placeholder
               ], argb.white)
            , (spad "Position: (%s, %s, %s)",
               [ (#f32, printf_val with f32 = s.eye.position.x)
               , (#f32, printf_val with f32 = s.eye.position.y)
               , (#f32, printf_val with f32 = s.eye.position.z)
               ], argb.white)
            , (spad "Orientation: (%s, %s, %s)",
               [ (#f32, printf_val with f32 = s.eye.orientation.x)
               , (#f32, printf_val with f32 = s.eye.orientation.y)
               , (#f32, printf_val with f32 = s.eye.orientation.z)
               ], argb.white)
            , (spad "View dist.: %s",
               [ (#f32, printf_val with f32 = s.screen_view_dest)
               , printf_placeholder
               , printf_placeholder
               ], argb.white)
            ]

  let mouse _ _ _ s = s
  let wheel _ _ s = s
}
