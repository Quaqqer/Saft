fn main() {
    let parse_result = saft::parser::AstParser::new().parse("hello world");
    println!("{:?}", parse_result);
}
