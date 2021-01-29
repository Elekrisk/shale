#![feature(assoc_char_funcs)]

mod parser;
mod ast;
mod types;

fn main() {
    let code = std::fs::read_to_string("test.shale").unwrap();
    unsafe { CODE = std::str::from_utf8_unchecked(std::slice::from_raw_parts(code.as_ptr(), code.len())) };
    match parser::parser::program_traced(&code) {
        Ok(_) => (),
        Err(e) => println!("{}", e)
    }
}

static mut CODE: &str = "";

fn to_row_col(index: usize) -> (usize, usize) {
    unsafe {
        let mut pos = (1, 1);
        let mut chars = CODE.chars();
        for _ in 0..index {
            if chars.next().unwrap() == '\n' {
                pos = (pos.0 + 1, 1);
            } else {
                pos = (pos.0, pos.1 + 1);
            }
        }
        pos
    }
}
