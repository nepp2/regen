/// Bare bones C foreign function interface
/// 
/// This is currently based on the observation that windows C calls
/// seem to pass all parameters and return values in 64 bit registers.
/// When something is bigger than 64 bit, a pointer is passed instead.
//

#[test]
fn ffi_test() {
  let fun = blah as *const ();
  unsafe {
    call_6(fun, 1, 2, 3, 4, 5, 6);
  }
}

pub extern "C" fn blah(a : i16, b : i16, c : u8, d : u32, e : u8, f : u8) {
  println!(
    "a: {}, b: {}, c: {}, d: {}, e: {}, f: {}",
    a, b, c, d, e, f);
}

pub unsafe fn call_c_function(fun : *const (), args : &[u64]) -> u64 {
  let a = args;
  match a.len() {
    0 => call_0(fun),
    1 => call_1(fun, a[0]),
    2 => call_2(fun, a[0], a[1]),
    3 => call_3(fun, a[0], a[1], a[2]),
    4 => call_4(fun, a[0], a[1], a[2], a[3]),
    5 => call_5(fun, a[0], a[1], a[2], a[3], a[4]),
    6 => call_6(fun, a[0], a[1], a[2], a[3], a[4], a[5]),
    7 => call_7(fun, a[0], a[1], a[2], a[3], a[4], a[5], a[6]),
    8 => call_8(fun, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]),
    9 => call_9(fun, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]),
    10 => call_10(fun, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]),
    _ => {
      panic!("C calls with {} arguments not supported", a.len())
    }
  }
}

unsafe fn call_0(fun : *const ()) -> u64 {
  let fun : extern "C" fn() -> u64 =
    std::mem::transmute(fun);
  fun()
}

unsafe fn call_1(fun : *const (), a : u64) -> u64 {
  let fun : extern "C" fn(u64) -> u64 =
    std::mem::transmute(fun);
  fun(a)
}

unsafe fn call_2(
  fun : *const (),
  a : u64, b : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b)
}

unsafe fn call_3(
  fun : *const (),
  a : u64, b : u64, c : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c)
}

unsafe fn call_4(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d)
}

unsafe fn call_5(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64, e : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d, e)
}

unsafe fn call_6(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64, e : u64, f : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d, e, f)
}

unsafe fn call_7(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64, e : u64, f : u64, g : u64
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d, e, f, g)
}

unsafe fn call_8(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64, e : u64, f : u64, g : u64, h : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64, u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d, e, f, g, h)
}

unsafe fn call_9(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64, e : u64, f : u64, g : u64,
  h : u64, i : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64, u64, u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d, e, f, g, h, i)
}

unsafe fn call_10(
  fun : *const (),
  a : u64, b : u64, c : u64, d : u64, e : u64, f : u64, g : u64,
  h : u64, i : u64, j : u64,
) -> u64
{
  let fun : extern "C" fn(u64, u64, u64, u64, u64, u64, u64, u64, u64, u64) -> u64 =
    std::mem::transmute(fun);
  fun(a, b, c, d, e, f, g, h, i, j)
}
