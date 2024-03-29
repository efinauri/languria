#[cfg(test)]
mod tests {
    use languria::environment::Environment;
    use languria::errors::ErrorScribe;
    use languria::errors::TerminationPolicy::STRICT;
    use languria::user_io::interpret_instructions;
    use std::fs::File;
    use std::io::Read;
    use std::path::PathBuf;

    #[test]
    fn readme() {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("README.md");
        let mut f = File::open(path).unwrap();
        let mut readme = String::new();
        f.read_to_string(&mut readme).unwrap();
        let mut instructions = vec![];
        let mut partial_instruction = String::new();
        let mut inside_code_block = false;
        for line in readme.split('\n') {
            if line.starts_with("```") {
                if inside_code_block {
                    instructions.push(partial_instruction.clone());
                    partial_instruction.clear();
                }
                inside_code_block = !inside_code_block;
                continue;
            }
            if inside_code_block {
                partial_instruction.push_str(line);
                partial_instruction.push('\n');
            }
        }
        let mut es = ErrorScribe::from_termination_policy(STRICT);
        let mut env = Environment::new();
        for ins in instructions {
            dbg!(&ins);
            assert!(interpret_instructions(
                &mut es,
                ins.to_string(),
                &mut env,
                false
            ))
        }
    }
}
