use std::collections::HashMap;
use std::hash::Hash;

pub struct Environment<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> Environment<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn begin_frame(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn end_frame(&mut self) {
        self.stack.pop();
    }

    pub fn insert(&mut self, key: K, value: V) {
        let last_frame = self.stack.len() - 1;
        self.stack[last_frame].insert(key, value);
    }

    /// Return the element, and how many frames above of the current frame it is.
    pub fn get(&self, key: &K) -> Option<(usize, &V)> {
        for (idx, frame) in self.stack.iter().rev().enumerate() {
            if let Some(v) = frame.get(key) {
                return Some((idx, v));
            }
        }

        None
    }

    pub fn size_current_frame(&self) -> usize {
        self.stack.last().unwrap().len()
    }
}
