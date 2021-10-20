use std::collections::{ HashMap, HashSet };
use crate::trans::*;
use crate::identifier::*;

pub enum MoveResult {
    Variable(Identifier),
    Member(Box<MoveResult>, Identifier),
    Right,
    Deref,
}

impl MoveResult {
    fn get_top(&self) -> Option<&Identifier> {
        match self {
            MoveResult::Right | MoveResult::Deref => None,
            MoveResult::Variable(ref id) => Some(id),
            MoveResult::Member(res, _) => res.as_ref().get_top(),
        }
    }
    fn get_tag(&self) -> Option<Tag> {
        match self {
            MoveResult::Right | MoveResult::Deref => None,
            MoveResult::Variable(ref id) => Some(id.tag.clone()),
            MoveResult::Member(_, id) => Some(id.tag.clone())
        }
    }
}

enum DeadElem {
    Tag(Tag),
    Member(DeadTree),
}

struct DeadTree {
    vars: HashMap<Identifier, DeadElem>,
}

impl DeadTree {
    fn new() -> Self {
        Self { vars: HashMap::new() }
    }
    fn is_empty(&self) -> bool {
        self.vars.is_empty()
    }
    fn move_result(&mut self, res: MoveResult) -> Result<(), String> {
        let mut top = res;
        let mut ids = Vec::new();
        while let MoveResult::Member(par, id) = top {
            ids.push(id);
            top = *par;
        }
        match top {
            MoveResult::Deref => { return Err(format!("deref value cannot be moved")); }
            MoveResult::Right => { return Ok(()); }
            MoveResult::Variable(id) => {
                ids.push(id);
            }
            MoveResult::Member(par, id) => unreachable!(),
        }
        let mut tree = self;
        while let Some(top) = ids.pop() {
            match tree.vars.get_mut(&top) {
                None => {
                    if ids.is_empty() {
                        tree.vars.insert(top, DeadElem::Tag(top.tag.clone()));
                        break
                    }
                    else {
                        let elem = tree.vars.entry(top).or_insert(DeadElem::Member(Self::new()));
                        if let DeadElem::Member(t) = elem { tree = t; }
                        else { unreachable!() }
                    }
                }
                Some(DeadElem::Tag(tag)) => {
                    return Err(format!("{:?} is dead by {:?}", top, tag));
                }
                Some(DeadElem::Member(ref mut t)) => {
                    tree = t;
                }
            }
        }
        Ok(())
    }
    fn be_alive_rec(&mut self, ids: Vec<Identifier>) -> Result<(), String> {
        if let Some(top) = ids.pop() {
            let remove = if ids.is_empty() {
                true
            } else {
                match self.vars.get_mut(&top) {
                    None => false,
                    Some(DeadElem::Tag(tag)) => {
                        return Err(format!("cannot partial alive"));
                    }
                    Some(DeadElem::Member(ref mut tree)) => {
                        tree.be_alive_rec(ids)?;
                        tree.is_empty()
                    }
                }
            };
            if remove {
                self.vars.remove(&top);
            }
        }
        else {
            unreachable!();
        }
        Ok(())
    }
    fn be_alive(&mut self, res: MoveResult) -> Result<(), String> {
        let mut top = res;
        let mut ids = Vec::new();
        while let MoveResult::Member(par, id) = top {
            ids.push(id);
            top = *par;
        }
        match top {
            MoveResult::Deref => { return Ok(()); }
            MoveResult::Right => { return Ok(()); }
            MoveResult::Variable(id) => {
                ids.push(id);
            }
            MoveResult::Member(par, id) => unreachable!(),
        }
        self.be_alive_rec(ids)
    }
    fn parallel_merge(&mut self, source: Self) {
        for (id, elem) in source.vars.into_iter() {
            match (self.vars.get_mut(&id), elem) {
                (None, elem) => {
                    self.vars.insert(id, elem);
                }
                (Some(DeadElem::Tag(_)), _) => {}
                (Some(elem), DeadElem::Tag(tag)) => {
                    *elem = DeadElem::Tag(tag);
                }
                (Some(DeadElem::Member(tree)), DeadElem::Member(right)) => {
                    tree.parallel_merge(right);
                }
            }
        }
    }
    fn solve_lazy(&mut self, source: Self) -> Result<(), String> {
        for (id, elem) in source.vars.into_iter() {
            match (self.vars.get_mut(&id), elem) {
                (None, elem) => {
                    self.vars.insert(id, elem);
                }
                (Some(DeadElem::Tag(_)), _) => {
                    return Err(format!("already moved"));
                }
                (Some(elem), DeadElem::Tag(tag)) => {
                    return Err(format!("partial moved"));
                }
                (Some(DeadElem::Member(tree)), DeadElem::Member(right)) => {
                    tree.solve_lazy(right)?;
                }
            }
        }
        Ok(())
    }
}

pub struct VariablesMoveChecker {
    vars: HashMap<Identifier, Tag>,
    local: DeadTree,
    lazy: DeadTree,
    moved: HashSet<Tag>,
}

impl VariablesMoveChecker {
    pub fn new() -> Self {
        VariablesMoveChecker {
            vars: HashMap::new(),
            local: DeadTree::new(),
            lazy: DeadTree::new(),
            moved: HashSet::new(),
        }
    }
    pub fn regist_var(&mut self, var: &Identifier) {
        self.vars.insert(var.clone(), var.tag.clone());
    }
    pub fn move_result(&mut self, res: MoveResult) -> Result<(), String> {
        if let Some(id) = res.get_top() {
            let tag = res.get_tag().unwrap();
            self.moved.insert(tag);
            if self.vars.contains_key(id) {
                self.local.move_result(res)
            }
            else {
                self.lazy.move_result(res)
            }
        }
        else {
            Ok(())
        }
    }
    pub fn live_result(&mut self, res: MoveResult) -> Result<(), String> {
        if let Some(id) = res.get_top() {
            if self.vars.contains_key(id) {
                self.local.be_alive(res)
            }
            else {
                self.lazy.be_alive(res)
            }
        }
        else {
            Ok(())
        }
    }
    pub fn parallel_merge(&mut self, right: Self) {
        self.moved.extend(right.moved.into_iter());
        self.lazy.parallel_merge(right.lazy);
    }
    pub fn solve_lazys(&mut self, right: Self) -> Result<(), String> {
        self.moved.extend(right.moved.into_iter());

        for (id, elem) in right.lazy.vars.into_iter() {
            let tree = if self.vars.contains_key(&id) { &mut self.local } else { &mut self.lazy };
            match (tree.vars.get_mut(&id), elem) {
                (None, elem) => {
                    tree.vars.insert(id, elem);
                }
                (Some(DeadElem::Tag(_)), _) => {
                    return Err(format!("already moved"));
                }
                (Some(elem), DeadElem::Tag(tag)) => {
                    return Err(format!("partial moved"));
                }
                (Some(DeadElem::Member(tree)), DeadElem::Member(right)) => {
                    tree.solve_lazy(right)?;
                }
            }
        }
        Ok(())
    }
}


pub trait MoveCheck {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String>;
}
