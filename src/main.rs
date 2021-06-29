use std::env;
use std::process;

fn main() {
    if let Some(filename) = env::args().nth(1) {
    } else {
        eprintln!("Usage: rchef <filename>");
        process::exit(1);
    }
}
