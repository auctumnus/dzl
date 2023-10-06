#![warn(clippy::pedantic)]
mod parse;

fn main() {
    let mut parser = parse::Parser::from("if x { 1 1 }");
    let ast = parser.if_expr();
    println!("{ast:#?}");
}
