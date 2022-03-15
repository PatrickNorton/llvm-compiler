use std::collections::HashSet;

#[derive(Debug)]
pub struct DivergingInfo {
    will_return: bool,
    may_return: bool,
    will_break: HashSet<usize>,
    may_break: HashSet<usize>,
    will_continue: bool,
    may_continue: bool,
}

impl DivergingInfo {
    pub fn new() -> Self {
        Self {
            will_return: false,
            may_return: false,
            will_break: HashSet::new(),
            may_break: HashSet::new(),
            will_continue: false,
            may_continue: false,
        }
    }

    pub fn and_with(&mut self, other: DivergingInfo) {
        self.will_return &= other.will_return;
        self.may_return |= other.may_return;
        self.will_break.retain(|x| other.will_break.contains(x));
        self.may_break.extend(other.may_break);
        self.will_continue &= other.will_continue;
        self.may_continue |= other.may_continue;
    }

    pub fn or_with(&mut self, other: DivergingInfo) {
        self.will_return |= other.will_return;
        self.may_return |= other.may_return;
        self.will_break.extend(other.will_break);
        self.may_break.extend(other.may_break);
        self.will_continue |= other.will_continue;
        self.may_continue |= other.may_continue;
    }

    pub fn known_return(&mut self) {
        self.will_return = true;
        self.may_return = true;
    }

    pub fn possible_return(&mut self) {
        self.may_return = true;
    }

    pub fn known_break(&mut self, level: usize) {
        self.will_break.insert(level);
        self.may_break.insert(level);
    }

    pub fn possible_break(&mut self, level: usize) {
        self.may_break.insert(level);
    }

    pub fn known_continue(&mut self) {
        self.will_continue = true;
        self.may_continue = true;
    }

    pub fn possible_continue(&mut self) {
        self.may_continue = true;
    }

    pub fn make_uncertain(&mut self) {
        self.will_return = false;
        self.will_break.clear();
        self.will_continue = false;
    }

    pub fn will_diverge(&self) -> bool {
        self.will_return || self.will_continue || !self.will_break.is_empty()
    }

    pub fn may_diverge(&self) -> bool {
        self.may_return || self.may_continue || !self.may_break.is_empty()
    }

    pub fn will_return(&self) -> bool {
        self.will_return
    }

    pub fn may_return(&self) -> bool {
        self.may_return
    }

    pub fn will_break(&self) -> bool {
        !self.will_break.is_empty()
    }

    pub fn may_break(&self) -> bool {
        !self.may_break.is_empty()
    }

    pub fn may_continue(&self) -> bool {
        self.may_continue
    }
}

impl Default for DivergingInfo {
    fn default() -> Self {
        Self::new()
    }
}
