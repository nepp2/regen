
fn call_c_function() {
  let fun = blah as *const ();
  unsafe {
    call_6(fun, 1, 2, 3, 4, 5, 6);
  }
}

#[test]
fn ffi_test() {
  call_c_function();
}

pub extern "C" fn blah(a : i16, b : i16, c : u8, d : u32, e : u8, f : u8) {
  println!(
    "a: {}, b: {}, c: {}, d: {}, e: {}, f: {}",
    a, b, c, d, e, f);
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
