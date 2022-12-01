use std::collections::HashMap;
use std::hash::Hash;

struct Environment<K, V> {
    stack: Vec<HashMap<K, V>>,
    references: HashMap<K, Vec<usize>>,
}

impl<K, V> Environment<K, V>
where
    K: Hash + Eq + Clone,
    V: Clone,
{
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            references: HashMap::new(),
        }
    }

    fn begin_frame(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn end_frame(&mut self) {
        self.stack.pop();
    }

    fn insert_declaration(&mut self, key: K, value: V) -> Result<(), V> {
        let key_references = self.references.entry(key.clone()).or_default();
        let last_frame = self.stack.len() - 1;
        match key_references.last() {
            Some(&last) if last == last_frame => {
                Err(self.stack[last_frame][&key].clone())
            }
            _ => {
                key_references.push(last_frame);
                self.stack[last_frame].insert(key, value);
                Ok(())
            }
        }
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.references
            .get(key)
            .and_then(|r| self.stack[*r.last()?].get(key))
    }
}
