//! This module provides a generic infrastructure for exploring a search space in depth-first mode,
//! and iterating over all the solutions.
//!
//! The exploration is performed by the generic [`Backtracker`] type,
//! which implements the [`Iterator`](std::iter::Iterator) trait.
//!
//! The problem-specific features of the search space are implemented via the [`Choices`] trait.

/// A type implementing [`Choices`] provides the application-logic of a specific search space.
///
/// It encodes a particular state in the search space,
/// and provides access to the next state in the depth-first search.
pub trait Choices: Sized {
    /// The type of the solutions that are searched for.
    type Output;
    /// Return the next state in the depth first search:
    /// * `None` if this state is a dead-end,
    /// * a [`Tree::Leaf`] if this state is a solution,
    /// * a [`Tree::Children`] with the next state to explore otherwise,
    ///   i.e. either the next sibbling of the first child of this state.
    fn next(&mut self) -> Option<Tree<Self>>;
}

/// A helper enum to convey the result of [`Choices::next`].
pub enum Tree<C: Choices> {
    Children(C),
    Leaf(C::Output),
}

/// The engine of a depth-first search on the search space represented by `C`,
/// iterating over all the solutions.
pub struct Backtracker<C: Choices> {
    stack: Vec<C>,
}

impl<C: Choices> Backtracker<C> {
    /// Start the search at the given state `c`.
    pub fn new(c: C) -> Self {
        Backtracker { stack: vec![c] }
    }
}

impl<C: Choices> Iterator for Backtracker<C> {
    type Item = C::Output;

    fn next(&mut self) -> Option<C::Output> {
        match self.stack.last_mut() {
            Some(it) => match it.next() {
                Some(Tree::Leaf(t)) => Some(t),
                Some(Tree::Children(c)) => {
                    self.stack.push(c);
                    self.next()
                }
                None => {
                    self.stack.pop();
                    self.next()
                }
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryInto;

    use super::*;

    #[test]
    fn queens3() {
        let b = Backtracker::new(<Queen<3>>::new());
        let v: Vec<_> = b.collect();
        assert_eq!(v.len(), 0);
    }

    #[test]
    fn queens4() {
        let b = Backtracker::new(<Queen<4>>::new());
        let v: Vec<_> = b.collect();
        assert_eq!(v.len(), 2);
    }

    #[test]
    fn queens5() {
        let b = Backtracker::new(<Queen<5>>::new());
        let v: Vec<_> = b.collect();
        assert_eq!(v.len(), 10);
    }

    #[test]
    fn queens8() {
        let b = Backtracker::new(<Queen<8>>::new());
        let v: Vec<_> = b.collect();
        assert_eq!(v.len(), 92);
    }

    /// A configuation of the N-Queen problem.
    /// The configuration can be partial or total, correct or incorrect.
    #[derive(Clone, Debug)]
    struct Queen<const N: usize>(Vec<usize>);

    impl<const N: usize> Queen<N> {
        /// Build a new configuration, with one queen at the top-left corner.
        fn new() -> Self {
            assert!(N > 0);
            Queen(vec![0])
        }

        /// Build a new configuration as a copy of self,
        /// with an extra queen at the start of a new row.
        fn extend(&self) -> Self {
            assert!(self.0.len() < N);
            let mut v = Vec::with_capacity(self.0.len() + 1);
            v.extend(self.0.iter().copied());
            v.push(0);
            Queen(v)
        }

        /// Check whether the queen on the last row conflicts with any previous queen.
        ///
        /// Note that the previous queens are *not* checked with each other.
        fn check_last(&self) -> bool {
            let ilast = self.0.len() - 1;
            let vlast = self.0[ilast];

            for (i, v) in self.0[..ilast].iter().copied().enumerate() {
                if v == vlast || v + (ilast - i) == vlast || v == vlast + (ilast - i) {
                    return false;
                }
            }
            true
        }
    }

    /// Impl Choices to make Queen<N> usable with Backtracker.
    /// Note that the internal state of this type always represent the *next* state to explore.
    impl<const N: usize> Choices for Queen<N> {
        type Output = [usize; N];

        fn next(&mut self) -> Option<Tree<Self>> {
            let last = self.0.len() - 1;
            if self.0[last] == N {
                // The queen on the last row has been pushed beyond the last column,
                // so this state is a dead end.
                None
            } else {
                if self.check_last() {
                    // This configuration is correct.
                    if self.0.len() == N {
                        // This configuration is complete,
                        // so this state is a solution.

                        //println!("leaf {:?}", self);
                        let mut v = vec![N]; // future state ensuring a backtrack
                        std::mem::swap(&mut v, &mut self.0);
                        Some(Tree::Leaf(v.try_into().unwrap()))
                    } else {
                        // This configuration is incomplete,
                        // so this state is an intermediary node.

                        //println!("extend {:?}", self);A
                        let r = Some(Tree::Children(self.extend()));
                        // Shift the last queen to the right, in preparation for the next iteration.
                        self.0[last] += 1;
                        r
                    }
                } else {
                    // This configuration is incorrect.
                    // Shift the last queen to the right, and try again.
                    self.0[last] += 1;
                    self.next()
                }
            }
        }
    }
}
