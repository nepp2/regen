
def game = struct {
  p : pos,
};

def game_state = init_game();

def window = create_sdl_window();
def mouse = sdl_mouse_state(window); // this is a state container
def ctx = render_context(window); // this is an output

// depends on game_state and mouse
def update = fun() {
  game_state.p = mouse.pos;
};

// depends on game_state and ctx
def render = fun() {
  let g = &game_state;
  clear(ctx);
  draw_square(ctx, g.pos.x, g.pos.y, 5);
};

def game_loop = fun() {
  update()
  render();
};

bind_handler(timer_millis(16), game_loop)

// how do i make it clear that render can be hotloaded without recalculating game state?
