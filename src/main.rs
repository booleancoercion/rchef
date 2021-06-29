use std::env;
use std::process;

fn main() {
    if let Some(filename) = env::args().nth(1) {
        if let Err(why) = rchef::run(&filename) {
            eprintln!("Error: {}", why);
        }
    } else {
        eprintln!("Usage: rchef <filename>");
        process::exit(1);
    }
}
