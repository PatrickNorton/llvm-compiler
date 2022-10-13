use std::collections::HashSet;

/// Information on whether or not a statement diverges.
///
/// # Types of divergence
///
/// A statement can currently diverge in 3 currently tracked ways: returning,
/// breaking, and continuing.
///
/// A `return` divergence corresponds to `return` statements, as well as `throw`
/// statements. Exception-throwing is not tracked separately here because there
/// are no currently-checked invariants that rely on exceptions being any
/// different than a `return` that leaves the function. FIXME: This actually
/// isn't true, try-catch blocks break this.
///
/// A `break` divergence corresponds to the `break` statement. These are tracked
/// separately by each level, since different levels of breaks result in
/// divergence to different places.
///
/// A `continue` divergence corresponds to the `continue` statement.
///
/// # Divergence states
///
/// Each of the tracked diverges have three states: "will", "may", and "won't".
///
/// If a divergence *will* happen, then it is guaranteed under all possible code
/// paths. In this case, the compiler may assume that it is impossible to access
/// any code past the current statement.
///
/// If a divergence *may* happen, then there exists at least one possible code
/// path (within the knowledge of the compiler, doing this to perfect accuracy
/// would require solving the halting problem) diverges. This is particularly
/// useful in while-true loops, where knowing if a possible `break` exists
/// changes the semantics of the code.
///
/// If a divergence *will not* happen, then there are no possible code paths
/// that diverge in that manner.
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
    /// Creates a new `DivergingInfo` that represents a non-diverging statement.
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

    /// Computes the logical "and" of `self` and another `DivergingInfo`.
    ///
    /// Note that this is not the logical "and" on the *statement* level, but on
    /// the *value* level, e.g. this computes `self.will_return &
    /// other.will_return`, as opposed to "the diverging info when statement 1
    /// *and* statement 2 run". This is intended for use with multiple mutually
    /// exclusive branches, for example `if ... elif ... else` chains.
    pub fn and_with(&mut self, other: DivergingInfo) {
        self.will_return &= other.will_return;
        self.may_return |= other.may_return;
        self.will_break.retain(|x| other.will_break.contains(x));
        self.may_break.extend(other.may_break);
        self.will_continue &= other.will_continue;
        self.may_continue |= other.may_continue;
    }

    /// Computes the logical "or" of `self` and another `DivergingInfo`.
    ///
    /// Note that this is not the logical "or" on the *statement* level, but on
    /// the *value* level, e.g. this computes `self.will_return |
    /// other.will_return`, as opposed to "the diverging info when statement 1
    /// *or* statement 2 run". This is intended for use with multiple
    /// consecutive statements.
    pub fn or_with(&mut self, other: DivergingInfo) {
        self.will_return |= other.will_return;
        self.may_return |= other.may_return;
        self.will_break.extend(other.will_break);
        self.may_break.extend(other.may_break);
        self.will_continue |= other.will_continue;
        self.may_continue |= other.may_continue;
    }

    /// Adds a known return to the current `DivergingInfo`.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_return();
    /// assert!(div.will_return());
    /// assert!(div.may_return());
    /// ```
    ///
    /// # See also
    ///
    /// If you want to query whether there is a known return, use
    /// [`Self::will_return`].
    pub fn known_return(&mut self) {
        self.will_return = true;
        self.may_return = true;
    }

    /// Adds a possible return to the current `DivergingInfo`.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.possible_return();
    /// assert!(div.may_return());
    /// ```
    ///
    /// # See also
    ///
    /// If you want to query whether there is a possible return, use
    /// [`Self::may_return`].
    pub fn possible_return(&mut self) {
        self.may_return = true;
    }

    /// Adds a known break to the current `DivergingInfo`.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_break(0);
    /// assert!(div.will_break());
    /// assert!(div.may_break());
    /// ```
    ///
    /// # See also
    ///
    /// If you want to query whether there is a possible break, use
    /// [`Self::will_break`].
    pub fn known_break(&mut self, level: usize) {
        self.will_break.insert(level);
        self.may_break.insert(level);
    }

    /// Adds a possible break to the current `DivergingInfo`.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.possible_break(0);
    /// assert!(div.may_break());
    /// ```
    ///
    /// # See also
    ///
    /// If you want to query whether there is a possible break, use
    /// [`Self::may_break`].
    pub fn possible_break(&mut self, level: usize) {
        self.may_break.insert(level);
    }

    /// Adds a known continue to the current `DivergingInfo`.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(div.will_continue());
    /// assert!(div.may_continue());
    /// ```
    ///
    /// # See also
    ///
    /// If you want to query whether there is a possible continue, use
    /// [`Self::will_continue`].
    pub fn known_continue(&mut self) {
        self.will_continue = true;
        self.may_continue = true;
    }

    /// Adds a possible continue to the current `DivergingInfo`.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.possible_continue();
    /// assert!(div.may_continue());
    /// ```
    ///
    /// # See also
    ///
    /// If you want to query whether there is a possible continue, use
    /// [`Self::may_continue`].
    pub fn possible_continue(&mut self) {
        self.may_continue = true;
    }

    /// Makes the diverging info uncertain.
    ///
    /// This changes the status such that there are no longer any certain
    /// divergences; only uncertain ones remain. This is intended for places
    /// like loops that may execute zero times or if statements that may not
    /// run. This is roughly equivalent to
    /// ```
    /// self.and_with(DivergingInfo::new())
    /// ```
    pub fn make_uncertain(&mut self) {
        self.will_return = false;
        self.will_break.clear();
        self.will_continue = false;
    }

    /// Returns `true` if there is a known diverge of any type.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(div.will_diverge());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_return();
    /// assert!(div.will_diverge());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_return();
    /// assert!(!div.will_diverge());
    /// ```
    pub fn will_diverge(&self) -> bool {
        self.will_return || self.will_continue || !self.will_break.is_empty()
    }

    /// Returns `true` if there is a possible diverge of any type.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.possible_continue();
    /// assert!(div.may_diverge());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_return();
    /// assert!(div.may_diverge());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_return();
    /// assert!(div.may_diverge());
    /// ```
    pub fn may_diverge(&self) -> bool {
        self.may_return || self.may_continue || !self.may_break.is_empty()
    }

    /// Returns `true` if there is a known return.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_return();
    /// assert!(div.will_return());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_return();
    /// assert!(!div.will_return());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(!div.will_return());
    /// ```
    pub fn will_return(&self) -> bool {
        self.will_return
    }

    /// Returns `true` if there is a possible return.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_return();
    /// assert!(div.may_return());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_return();
    /// assert!(div.may_return());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(!div.may_return());
    /// ```
    pub fn may_return(&self) -> bool {
        self.may_return
    }

    /// Returns `true` if there is a known break.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_break(0);
    /// assert!(div.will_break());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_break(0);
    /// assert!(!div.will_break());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(!div.will_break());
    /// ```
    pub fn will_break(&self) -> bool {
        !self.will_break.is_empty()
    }

    /// Returns `true` if there is a possible return.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_break(0);
    /// assert!(div.may_break());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_break(0);
    /// assert!(div.may_break());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(!div.may_break());
    /// ```
    pub fn may_break(&self) -> bool {
        !self.may_break.is_empty()
    }

    /// Returns `true` if there is a possible continue.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// div.known_continue();
    /// assert!(div.may_continue());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.possible_continue();
    /// assert!(div.may_continue());
    ///
    /// let mut info = DivergingInfo::new();
    /// div.known_return();
    /// assert!(!div.may_continue());
    /// ```
    pub fn may_continue(&self) -> bool {
        self.may_continue
    }

    /// Decrease the counters of all breaks by 1, removing them if they are 0.
    ///
    /// The intended use for this is in [`LoopConverter`], where, when exiting a
    /// loop, the break labels need to be adjusted.
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// info.known_break(0);
    /// assert!(info.will_break());
    /// info.decrement_breaks();
    /// assert!(!info.will_break());
    ///
    /// info.known_break(2);
    /// assert!(info.will_break());
    /// info.decrement_breaks();
    /// assert!(info.will_break());  // Now at level 1
    /// info.decrement_breaks();
    /// assert!(info.will_break());  // Now at level 0
    /// info.decrement_breaks();
    /// assert!(!info.will_break()); // Now gone
    /// ```
    ///
    /// # See also
    /// [`Self::clear_continue`]
    pub fn decrement_breaks(&mut self) {
        // TODO: Reuse allocations
        self.will_break = self
            .will_break
            .iter()
            .filter_map(|&x| x.checked_sub(1))
            .collect();
        self.may_break = self
            .may_break
            .iter()
            .filter_map(|&x| x.checked_sub(1))
            .collect();
    }

    /// Removes any known or potential continues from the [`DivergingInfo`].
    ///
    /// # Examples
    /// ```
    /// let mut info = DivergingInfo::new();
    /// info.known_continue();
    /// assert!(info.will_continue());
    /// info.clear_continue();
    /// assert!(!info.will_continue());
    /// assert!(!info.may_continue());
    ///
    /// info.possible_continue();
    /// assert!(info.may_continue());
    /// info.clear_continue();
    /// assert!(!info.may_continue());
    /// ```
    pub fn clear_continue(&mut self) {
        self.will_continue = false;
        self.may_continue = false;
    }
}

impl Default for DivergingInfo {
    fn default() -> Self {
        Self::new()
    }
}
