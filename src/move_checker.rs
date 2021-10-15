use std::collections::HashMap;

use crate::identifier::*;

pub struct VariablesMoveChecker<> {
    vars: HashMap<Identifier, Tag>,
    moved: HashMap<Identifier, Tag>,
    lazy: HashMap<Identifier, Tag>,
}

impl VariablesMoveChecker {
    pub fn new() -> Self {
        VariablesMoveChecker {
            vars: HashMap::new(),
            moved: HashMap::new(),
            lazy: HashMap::new(),
        }
    }
    pub fn regist_var(&mut self, var: &Identifier) {
        self.vars.insert(var.clone(), var.tag.clone());
    }
    pub fn move_var(&mut self, var: &Identifier) -> Result<(), String> {
        if let Some((i, _)) = self.vars.remove_entry(var) {
            self.moved.insert(i, var.tag.clone());
            Ok(())
        }
        else if let Some(tag) = self.moved.get(var) {
            Err(format!("cant move {:?}, its moved {:?}", var, tag))
        }
        else if let Some(tag) = self.lazy.get(var) {
            Err(format!("cant move {:?}, its moved {:?}", var, tag))
        }
        else {
            self.lazy.insert(var.clone(), var.tag.clone());
            Ok(())
        }
    }
    pub fn parallel_merge(&mut self, right: Self) {
        self.moved.extend(right.moved.into_iter());
        self.lazy.extend(right.lazy.into_iter());
    }
    pub fn solve_lazys(&mut self, right: Self) -> Result<(), String> {
        self.moved.extend(right.moved.into_iter());
        for (i, _t) in right.lazy.into_iter() {
            self.move_var(&i)?;
        }
        Ok(())
    }
}

pub enum MoveResult {
    Movable(Identifier),
    Right,
}

trait MoveCheck {
    fn move_check(&self, mc: &mut VariablesMoveChecker) -> Result<MoveResult, String>;
}
