use std::collections::HashMap;

use super::ast::Ident;

pub type Id = usize;

pub struct Tree {
    /// id => parent, depth, children
    graph: Vec<(Id, usize, Vec<Id>)>,
    /// ident => id in the tree
    assoc: HashMap<Ident, Id>,
}

impl Tree {
    pub fn new() -> Self {
        Self {
            graph: vec![(0, 0, Vec::new())],
            assoc: HashMap::new(),
        }
    }

    pub const fn root(&self) -> Id {
        0
    }

    pub fn depth(&self, node: Id) -> usize {
        self.graph[node].1
    }

    pub fn add_child(&mut self, parent: Id, name: Ident) -> Id {
        let id = self.graph.len();
        self.graph.push((parent, self.depth(parent), Vec::new()));
        self.graph[parent].2.push(id);
        self.assoc.insert(name, id);
        id
    }

    pub fn parent(&self, child: Id) -> Option<Id> {
        let parent = self.graph[child].0;
        (child != 0).then_some(parent)
    }

    pub fn children(&self, parent: Id) -> &[Id] {
        &self.graph[parent].2
    }

    pub fn find_by_name(&self, name: Ident) -> Option<Id> {
        self.assoc.get(&name).copied()
    }

    /// Return the relative height difference of callee_func and its lca with caller_func
    /// called_func should be a direct ancestor or son-of-an-ancestor of caller_func
    pub fn lca(&self, caller_func: Id, called_func: Id) -> usize {
        let mut node = caller_func;
        while called_func != node
            && (self
                .children(node)
                .iter()
                .find(|c| **c == called_func)
                .is_none())
        {
            // safe because of the precondition
            node = self.parent(node).unwrap();
        }
        self.depth(node) - self.depth(caller_func)
    }
}
