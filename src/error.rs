pub fn error(line: usize, message: &String) {
    report(line, &String::from(""), message);
}

fn report(line: usize, location: &String, message: &String) -> bool {
    eprintln!("[line {}] Error{}: {}", line, location, message);
    true
}
