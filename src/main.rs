#![warn(clippy::pedantic)]
mod parse;

fn main() {
    let program = r#"let foo = (x: int) => x * 2
    let foo = (x: int) -> int => x * 2
    let baz = (x: int) -> int => {
        return x * 2
    }
    const main = () => {
        foreachint([1, 2, 3, 4], (n) => stdout.println(n * 2))
    }
    "#;

    let mut parser = parse::Parser::from(program);
    let (statements, errors) = parser.program();

    println!("{statements:#?}");
    println!("{errors:#?}");

    // let mut parser = parse::Parser::from(r#"const x = 1;
    // const y = 2;
    // const z = x + y;
    // "#);
    // let mut statements = Vec::new();
    // while let Ok(ast) = parser.statement() {
    //     statements.push(ast);
    //     parser.next_while(|c| c.is_whitespace());
    // }
    // println!("{:#?}", statements);
}
