use std::io::Write;

mod error;
mod expr;
mod literal;
mod op;
mod parser;
mod scanner;
mod token;

fn run_file(path: &String) {
    let source = std::fs::read_to_string(path).expect("Unable to read file");
    run(&source)
}

fn run_prompt() {
    println!("Welcome to the Nox interpreter! To exit type \"exit()\".");
    loop {
        print!(">>> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        std::io::stdin()
            .read_line(&mut line)
            .expect("Failed to read from stdin");
        if line.trim() == "exit()" {
            break;
        }
        run(&line);
    }
}

fn run(source: &String) {
    let mut scanner = scanner::Scanner::new(source.to_string());
    let tokens = scanner.scan_tokens();

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    let mut parser = parser::Parser::new(tokens.to_vec());
    let expr = parser.parse();

    println!("{:?}", expr);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    for arg in args.iter() {
        println!("{}", arg);
    }
    match args.len() {
        len if len > 2 => {
            println!("Useage: rnox [script]");
            std::process::exit(64);
        }
        2 => run_file(&args[0]),
        _ => run_prompt(),
    }
}
