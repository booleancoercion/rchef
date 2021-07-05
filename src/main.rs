use clap::{App, Arg};

use std::process;

fn main() {
    let matches = App::new("rchef")
        .version("0.1")
        .author("boolean_coercion <booleancoercion@gmail.com>")
        .about("A fully-featured interpreter for the esoteric programming language Chef.")
        .arg(
            Arg::new("spaced")
                .about("Determines whether your program will run in spaced mode.")
                .short('s')
                .long("spaced"),
        )
        .arg(
            Arg::new("filename")
                .about("The filename of the program you intend to run.")
                .required(true),
        )
        .get_matches();

    let filename = matches.value_of("filename").unwrap(); // filename is required
    let spaced = matches.is_present("spaced");

    if let Err(why) = rchef::run(filename, spaced) {
        eprintln!("{}", why);
        process::exit(1);
    }
}
