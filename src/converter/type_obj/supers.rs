use std::iter::FusedIterator;

use once_cell::sync::OnceCell;

use super::TypeObject;

#[derive(Debug)]
pub struct SuperHolder {
    pub(super) supers: OnceCell<Vec<TypeObject>>,
    pub(super) fulfilled_interfaces: OnceCell<Vec<TypeObject>>,
}

#[derive(Debug, Copy, Clone)]
pub struct SuperRef<'a> {
    supers: &'a [TypeObject],
    fulfilled_interfaces: &'a [TypeObject],
}

#[derive(Debug)]
pub struct SuperRefIter<'a> {
    supers: std::slice::Iter<'a, TypeObject>,
    fulfilled: std::slice::Iter<'a, TypeObject>,
}

impl SuperHolder {
    pub fn new(supers: OnceCell<Vec<TypeObject>>) -> Self {
        Self {
            supers,
            fulfilled_interfaces: OnceCell::new(),
        }
    }

    pub fn reference(&self) -> SuperRef<'_> {
        SuperRef {
            supers: self.supers.get().map(|x| x.as_slice()).unwrap_or_default(),
            fulfilled_interfaces: self
                .fulfilled_interfaces
                .get()
                .map(|x| x.as_slice())
                .unwrap_or_default(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ TypeObject> {
        self.supers
            .get()
            .into_iter()
            .flatten()
            .chain(self.fulfilled_interfaces.get().into_iter().flatten())
    }
}

impl<'a> SuperRef<'a> {
    pub fn from_slice(slice: &'a [TypeObject]) -> Self {
        Self {
            supers: slice,
            fulfilled_interfaces: &[],
        }
    }

    pub fn first(self) -> Option<&'a TypeObject> {
        self.supers
            .first()
            .or_else(|| self.fulfilled_interfaces.first())
    }
}

impl<'a> IntoIterator for SuperRef<'a> {
    type Item = &'a TypeObject;

    type IntoIter = SuperRefIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        SuperRefIter {
            supers: self.supers.iter(),
            fulfilled: self.fulfilled_interfaces.iter(),
        }
    }
}

impl<'a> Iterator for SuperRefIter<'a> {
    type Item = &'a TypeObject;

    fn next(&mut self) -> Option<Self::Item> {
        self.supers.next().or_else(|| self.fulfilled.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }

    fn last(self) -> Option<Self::Item> {
        self.fulfilled.last().or_else(|| self.supers.last())
    }

    fn count(self) -> usize {
        self.fulfilled.count() + self.supers.count()
    }
}

impl<'a> FusedIterator for SuperRefIter<'a> {}

impl<'a> ExactSizeIterator for SuperRefIter<'a> {
    fn len(&self) -> usize {
        self.fulfilled.len() + self.supers.len()
    }
}
