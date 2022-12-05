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

    /// Return the element, and how many frames above of the current frame it is.
    fn get(&self, key: &K) -> Option<(usize, &V)> {
        self.references.get(key).and_then(|r| {
            let last_pos = *r.last()?;
            Some((
                self.stack.len() - last_pos - 1,
                self.stack[last_pos].get(key)?,
            ))
        })
    }

    fn size_current_frame(&self) -> usize {
	self.stack.last().unwrap().len()
    }
}
