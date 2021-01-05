use crate::xetex_ini::MEM;
fn next<'a>(p: &'a i32) -> &'a i32 {
    unsafe { &MEM[(*p as usize)].b32.s1 }
}

use crate::xetex_xetexd::{TeXInt, TeXOpt};
pub struct NodeList<'a>(&'a mut i32);
impl<'a> NodeList<'a> {
    pub fn from(p: &'a mut i32) -> Self {
        Self(p)
    }
    pub fn ptr(&self) -> Option<usize> {
        self.0.opt()
    }
    pub fn iter(&self) -> Iter<'_> {
        Iter::new(self.0)
    }
    pub fn iter_mut(&mut self) -> IterMut {
        IterMut::new(&mut self.0)
    }
    pub fn tail(&self) -> Option<usize> {
        self.iter().last()
    }
    pub fn is_empty(&self) -> bool {
        self.ptr().is_none()
    }
    pub fn len(&self) -> usize {
        self.iter().count()
    }
    pub fn push(&mut self, p: usize) -> usize {
        if let Some(tail) = self.iter_mut().last() {
            unsafe { MEM[tail].b32.s1 = Some(p).tex_int() }
        } else {
            *self.0 = Some(p).tex_int()
        }
        p
    }
}

impl<'a> IntoIterator for &'a NodeList<'a> {
    type Item = usize;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct Iter<'a>(&'a i32);

impl<'a> Iter<'a> {
    fn new(p: &'a i32) -> Self {
        Self(p)
    }
}

impl<'a> std::iter::Iterator for Iter<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(p) = self.0.opt() {
            self.0 = next(self.0);
            Some(p)
        } else {
            None
        }
    }
}

impl<'a> IntoIterator for &'a mut NodeList<'a> {
    type Item = usize;
    type IntoIter = IterMut<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

pub struct IterMut<'a>(&'a i32);

impl<'a> IterMut<'a> {
    fn new(p: &'a mut i32) -> Self {
        Self(p)
    }
}

impl<'a> std::iter::Iterator for IterMut<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(p) = self.0.opt() {
            self.0 = next(self.0);
            Some(p)
        } else {
            None
        }
    }
}
