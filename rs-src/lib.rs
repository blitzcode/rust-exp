
#[no_mangle]
pub extern fn rust_hello_word(input: i32) -> () {
    println!("Hello World: {}", input)
}

