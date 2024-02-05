#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    use std::path::PathBuf;
    use std::{fs, io};

    use languria::environment::Environment;
    use languria::errors::ErrorScribe;
    use languria::errors::TerminationPolicy::STRICT;
    use languria::user_io::interpret_instructions;

    #[test]
    fn interpret_files() {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("tests/resources");

        let test_files = fs::read_dir(path)
            .unwrap()
            .map(|res| res.map(|e| e.path()))
            .collect::<Result<Vec<_>, io::Error>>()
            .unwrap();

        let mut scribe = ErrorScribe::from_termination_policy(STRICT);
        let mut env = Environment::new();
        for f in test_files {
            dbg!(&f.file_name());
            let mut contents = String::new();
            File::open(f)
                .unwrap()
                .read_to_string(&mut contents)
                .unwrap();
            assert!(interpret_instructions(
                &mut scribe,
                contents,
                &mut env,
                false
            ));
        }
    }
}
