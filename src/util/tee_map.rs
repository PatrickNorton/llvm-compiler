use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

#[derive(Debug)]
struct TeeMapBuf<T, U, I, F> {
    backlog_a: VecDeque<T>,
    backlog_b: VecDeque<U>,
    iter: I,
    map: F,
}

#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
#[derive(Debug)]
pub struct LeftTeeMap<T, U, I, F> {
    rc_buffer: Rc<RefCell<TeeMapBuf<T, U, I, F>>>,
}

#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
#[derive(Debug)]
pub struct RightTeeMap<T, U, I, F> {
    rc_buffer: Rc<RefCell<TeeMapBuf<T, U, I, F>>>,
}

pub type TeeMapPair<T, U, I, F> = (LeftTeeMap<T, U, I, F>, RightTeeMap<T, U, I, F>);

pub trait TeeMap: Iterator {
    /// Map an iterator into two parts, which are then returned as separate iterators.
    ///
    /// The difference between this and [`tee`](Itertools::tee) is that the two
    /// iterators returned by `tee` yield the same elements, whereas these two iterators
    /// yield disjoint elements.
    ///
    /// # Examples
    ///
    /// ```
    /// let (mut a, mut b) = vec![1, 2, 3].into_iter().tee_map(|x| (x, -x));
    /// assert_eq!(a.collect(), vec![1, 2, 3]);
    /// assert_eq!(b.collect(), vec![-1, -2, -3]);
    /// ```
    fn tee_map<T, U, F>(self, func: F) -> TeeMapPair<T, U, Self, F>
    where
        F: FnMut(Self::Item) -> (T, U),
        Self: Sized,
    {
        let buffer = TeeMapBuf {
            backlog_a: VecDeque::new(),
            backlog_b: VecDeque::new(),
            iter: self,
            map: func,
        };
        let left = LeftTeeMap {
            rc_buffer: Rc::new(RefCell::new(buffer)),
        };
        let right = RightTeeMap {
            rc_buffer: left.rc_buffer.clone(),
        };
        (left, right)
    }
}

impl<T, U, I, F> Iterator for LeftTeeMap<T, U, I, F>
where
    I: Iterator,
    F: FnMut(I::Item) -> (T, U),
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buffer = self.rc_buffer.borrow_mut();
        match buffer.backlog_a.pop_front() {
            None => {}
            some_val => return some_val,
        }
        match buffer.iter.next() {
            None => None,
            Some(val) => {
                let (left, right) = (buffer.map)(val);
                buffer.backlog_b.push_back(right);
                Some(left)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let buffer = self.rc_buffer.borrow();
        let (min, max) = buffer.iter.size_hint();
        (
            min + buffer.backlog_a.len(),
            max.map(|x| x + buffer.backlog_a.len()),
        )
    }
}

impl<T, U, I, F> Iterator for RightTeeMap<T, U, I, F>
where
    I: Iterator,
    F: FnMut(I::Item) -> (T, U),
{
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buffer = self.rc_buffer.borrow_mut();
        match buffer.backlog_b.pop_front() {
            None => {}
            some_val => return some_val,
        }
        match buffer.iter.next() {
            None => None,
            Some(val) => {
                let (left, right) = (buffer.map)(val);
                buffer.backlog_a.push_back(left);
                Some(right)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let buffer = self.rc_buffer.borrow();
        let (min, max) = buffer.iter.size_hint();
        (
            min + buffer.backlog_b.len(),
            max.map(|x| x + buffer.backlog_b.len()),
        )
    }
}

impl<T> TeeMap for T where T: Iterator {}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::TeeMap;

    #[test]
    fn empty() {
        let (mut a, mut b) = iter::empty::<((), ())>().into_iter().tee_map(|x| x);
        assert_eq!(a.next(), None);
        assert_eq!(b.next(), None);
    }

    #[test]
    fn empty_size_hint() {
        let (a, b) = iter::empty::<((), ())>().into_iter().tee_map(|x| x);
        assert_eq!(a.size_hint(), (0, Some(0)));
        assert_eq!(b.size_hint(), (0, Some(0)));
    }

    #[test]
    fn at_once() {
        let (mut a, mut b) = vec![1, 2, 3].into_iter().tee_map(|x| (x, -x));
        assert_eq!(a.next(), Some(1));
        assert_eq!(a.next(), Some(2));
        assert_eq!(a.next(), Some(3));
        assert_eq!(a.next(), None);
        assert_eq!(b.next(), Some(-1));
        assert_eq!(b.next(), Some(-2));
        assert_eq!(b.next(), Some(-3));
        assert_eq!(b.next(), None);
    }

    #[test]
    fn interwoven() {
        let (mut a, mut b) = vec![1, 2, 3].into_iter().tee_map(|x| (x, -x));
        assert_eq!(a.next(), Some(1));
        assert_eq!(b.next(), Some(-1));
        assert_eq!(a.next(), Some(2));
        assert_eq!(b.next(), Some(-2));
        assert_eq!(a.next(), Some(3));
        assert_eq!(b.next(), Some(-3));
        assert_eq!(a.next(), None);
        assert_eq!(b.next(), None);
    }
}
