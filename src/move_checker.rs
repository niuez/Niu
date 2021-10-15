use std::collections::HashMap;
use crate::unify::TraitsInfo;
use crate::identifier::*;

pub struct VariablesMoveChecker {
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
    pub fn get_move_result<'a>(&self, var: &'a Identifier) -> MoveResult<'a> {
        if self.moved.contains_key(var) || self.lazy.contains_key(var) {
            MoveResult::Dead(var)
        }
        else {
            MoveResult::Movable(var)
        }
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
    pub fn move_result(&mut self, res: &MoveResult) -> Result<(), String> {
        match res {
            &MoveResult::Movable(ref var) => self.move_var(var),
            &MoveResult::Dead(ref var) => Err(format!("{:?} is moved", var)),
            _ => Ok(())
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

pub enum MoveResult<'a> {
    Movable(&'a Identifier),
    Dead(&'a Identifier),
    Right,
}

pub trait MoveCheck {
    fn move_check(&self, mc: &mut VariablesMoveChecker, trs: &TraitsInfo) -> Result<MoveResult, String>;
}
