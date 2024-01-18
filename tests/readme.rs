#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    #[test]
    fn readme() {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("tests/resources/readme.lgr");

        languria::user_input::interpret_file(path.to_str().unwrap(), true);
    }
}
