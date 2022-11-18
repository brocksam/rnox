use std::io::Write;

mod error;
mod expr;
mod interpreter;
mod literal;
mod op;
mod parser;
mod scanner;
mod status;
mod token;
mod value;

fn run_file(path: &String) {
    let source = std::fs::read_to_string(path).expect("Unable to read file");
    match run(&source) {
        status::Status::Success => {}
        status::Status::ParseError => std::process::exit(65),
        status::Status::RuntimeError => std::process::exit(70),
    }
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
        _ = run(&line);
    }
}

fn run(source: &String) -> status::Status {
    let mut scanner = scanner::Scanner::new(source.to_string());
    let tokens = scanner.scan_tokens();

    let mut parser = parser::Parser::new(tokens.clone());
    let result = parser.parse();

    let expr = match result {
        Ok(expr) => expr,
        Err(error) => {
            println!("{}", error);
            return status::Status::ParseError;
        }
    };

    let interpreter = interpreter::Interpreter::new(expr);
    let value = interpreter.interpret();
    match value {
        Ok(val) => println!("{}", val),
        Err(error) => {
            println!("{}", error);
            return status::Status::RuntimeError;
        }
    }
    status::Status::Success
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    for arg in &args {
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
