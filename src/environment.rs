use std::collections::HashMap;

use crate::{error::RuntimeError, value::Value};

pub struct Environment {
    values: std::collections::HashMap<String, Option<Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Option<Value>) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &String) -> Result<Value, RuntimeError> {
        match self.values.get(name) {
            Some(value) => match value {
                Some(v) => Ok(v.clone()),
                None => Err(RuntimeError {
                    message: format!("Found variable {} not bound to value.", name),
                }),
            },
            None => Err(RuntimeError {
                message: format!("Undefined variable {}.", name),
            }),
        }
    }
}
