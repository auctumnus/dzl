#![warn(clippy::pedantic)]
mod parse;

fn main() {
    let ast = parse::Parser::from("let x").statement();
    println!("{ast:#?}");
}
