pub struct Backtracker<C: Choices> {
    stack: Vec<C>,
}

pub trait Choices: Sized {
    type Output;
    fn next(&mut self) -> Option<Tree<Self>>;
}

pub enum Tree<C: Choices> {
    Children(C),
    Leaf(C::Output),
}

impl<C: Choices> Backtracker<C> {
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

    #[derive(Clone, Debug)]
    struct Queen<const N: usize>(Vec<usize>);

    impl<const N: usize> Queen<N> {
        fn new() -> Self {
            assert!(N > 0);
            Queen(vec![0])
        }

        fn extend(&self) -> Self {
            let mut v = Vec::with_capacity(self.0.len() + 1);
            v.extend(self.0.iter().copied());
            v.push(0);
            Queen(v)
        }

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

    impl<const N: usize> Choices for Queen<N> {
        type Output = Vec<usize>;

        fn next(&mut self) -> Option<Tree<Self>> {
            let last = self.0.len() - 1;
            if self.0[last] == N {
                None
            } else {
                if self.check_last() {
                    let r = if self.0.len() == N {
                        //println!("leaf {:?}", self);
                        Some(Tree::Leaf(self.0.clone()))
                    } else {
                        //println!("extend {:?}", self);
                        Some(Tree::Children(self.extend()))
                    };
                    self.0[last] += 1;
                    r
                } else {
                    self.0[last] += 1;
                    self.next()
                }
            }
        }
    }
}
