#[cfg(test)]
mod tests {
    use languria::environment::Environment;
    use languria::user_io::interpret_internal_files;

    #[test]
    fn interpret_files() {
        let mut env = Environment::new();
        interpret_internal_files("tests/resources", &mut env);
    }
}
