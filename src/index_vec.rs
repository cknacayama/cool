use std::{
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Range, RangeBounds, RangeFrom},
    vec::Drain,
};

pub trait Idx: Copy + 'static + Eq + PartialEq + Hash {
    fn index(self) -> usize;

    fn new(idx: usize) -> Self;

    #[inline]
    fn increment_by(&mut self, increment: usize) {
        *self = self.plus(increment);
    }

    #[inline]
    fn plus(self, increment: usize) -> Self {
        Self::new(self.index() + increment)
    }

    #[inline]
    fn minus(self, decrement: usize) -> Option<Self> {
        match self.index() {
            0 => None,
            index => Some(Self::new(index - decrement)),
        }
    }

    #[inline]
    fn next(&mut self) -> Self {
        let next = *self;
        *self = self.plus(1);
        next
    }

    #[inline]
    fn prev(self) -> Option<Self> {
        self.minus(1)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct IndexVec<I: Idx, V> {
    raw:     Vec<V>,
    _marker: PhantomData<I>,
}

impl<K: Idx, V> Debug for IndexVec<K, V>
where
    V: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.raw.iter()).finish()
    }
}

impl<K: Idx, V> IndexVec<K, V> {
    pub fn new() -> Self {
        Self {
            raw:     Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            raw:     Vec::with_capacity(cap),
            _marker: PhantomData,
        }
    }

    pub fn shrink_to_fit(&mut self) {
        self.raw.shrink_to_fit();
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = V>) {
        self.raw.extend(iter);
    }

    pub fn capacity(&self) -> usize {
        self.raw.capacity()
    }

    pub fn reverse(&mut self) {
        self.raw.reverse()
    }

    pub fn drain<R>(&mut self, range: R) -> Drain<V>
    where
        R: RangeBounds<usize>,
    {
        self.raw.drain(range)
    }

    pub fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn push(&mut self, value: V) {
        self.raw.push(value)
    }

    pub fn pop(&mut self) -> Option<V> {
        self.raw.pop()
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.raw.insert(key.index(), value)
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.raw.get(key.index())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.raw.get_mut(key.index())
    }

    pub fn retain<F: FnMut(&V) -> bool>(&mut self, f: F) {
        self.raw.retain(f)
    }

    pub fn retain_mut<F: FnMut(&mut V) -> bool>(&mut self, f: F) {
        self.raw.retain_mut(f)
    }

    pub fn iter(&self) -> Iter<K, V> {
        Iter {
            values:  self.raw.iter(),
            current: K::new(0),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        IterMut {
            values:  self.raw.iter_mut(),
            current: K::new(0),
        }
    }

    pub fn inner(&self) -> &[V] {
        &self.raw
    }

    pub fn indices(&self) -> impl Iterator<Item = K> {
        (0..self.raw.len()).map(K::new)
    }

    pub fn inner_mut(&mut self) -> &mut Vec<V> {
        &mut self.raw
    }

    pub fn into_inner(self) -> Vec<V> {
        self.raw
    }

    pub fn first(&self) -> Option<&V> {
        self.raw.first()
    }

    pub fn last(&self) -> Option<&V> {
        self.raw.last()
    }

    pub fn first_mut(&mut self) -> Option<&mut V> {
        self.raw.first_mut()
    }

    pub fn last_mut(&mut self) -> Option<&mut V> {
        self.raw.last_mut()
    }
}

impl<K: Idx, V> Default for IndexVec<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Idx, V> IntoIterator for IndexVec<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            values:  self.raw.into_iter(),
            current: K::new(0),
        }
    }
}

impl<'a, K: Idx, V> IntoIterator for &'a IndexVec<K, V> {
    type Item = (K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            values:  self.raw.iter(),
            current: K::new(0),
        }
    }
}

impl<'a, K: Idx, V> IntoIterator for &'a mut IndexVec<K, V> {
    type Item = (K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            values:  self.raw.iter_mut(),
            current: K::new(0),
        }
    }
}

impl<K: Idx, V> From<Vec<V>> for IndexVec<K, V> {
    fn from(values: Vec<V>) -> Self {
        Self {
            raw:     values,
            _marker: PhantomData,
        }
    }
}

impl<K: Idx, V> core::ops::Index<K> for IndexVec<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.raw[index.index()]
    }
}

impl<K: Idx, V> core::ops::IndexMut<K> for IndexVec<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.raw[index.index()]
    }
}

impl<K: Idx, V> core::ops::Index<Range<K>> for IndexVec<K, V> {
    type Output = [V];

    fn index(&self, index: Range<K>) -> &Self::Output {
        &self.raw[index.start.index()..index.end.index()]
    }
}

impl<K: Idx, V> core::ops::IndexMut<Range<K>> for IndexVec<K, V> {
    fn index_mut(&mut self, index: Range<K>) -> &mut Self::Output {
        &mut self.raw[index.start.index()..index.end.index()]
    }
}

impl<K: Idx, V> core::ops::Index<RangeFrom<K>> for IndexVec<K, V> {
    type Output = [V];

    fn index(&self, index: RangeFrom<K>) -> &Self::Output {
        &self.raw[index.start.index()..]
    }
}

impl<K: Idx, V> core::ops::IndexMut<RangeFrom<K>> for IndexVec<K, V> {
    fn index_mut(&mut self, index: RangeFrom<K>) -> &mut Self::Output {
        &mut self.raw[index.start.index()..]
    }
}

impl<'a, K: Idx, V> From<&'a IndexVec<K, V>> for &'a [V] {
    fn from(index_vec: &'a IndexVec<K, V>) -> Self {
        index_vec.raw.as_slice()
    }
}

impl<'a, K: Idx, V> From<&'a mut IndexVec<K, V>> for &'a mut [V] {
    fn from(index_vec: &'a mut IndexVec<K, V>) -> Self {
        index_vec.raw.as_mut_slice()
    }
}

impl<K: Idx, V> FromIterator<(K, V)> for IndexVec<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let values = iter.into_iter().map(|(_, v)| v).collect();
        Self {
            raw:     values,
            _marker: PhantomData,
        }
    }
}

impl<K: Idx, V> FromIterator<V> for IndexVec<K, V> {
    fn from_iter<I: IntoIterator<Item = V>>(iter: I) -> Self {
        Self {
            raw:     Vec::from_iter(iter),
            _marker: PhantomData,
        }
    }
}

impl Idx for usize {
    fn index(self) -> usize {
        self
    }

    fn new(index: usize) -> Self {
        index
    }
}

impl Idx for u32 {
    fn index(self) -> usize {
        self as usize
    }

    fn new(index: usize) -> Self {
        index as u32
    }
}

macro_rules! index_vec {
    () => (
        IndexVec::new()
    );
    ($elem:expr; $n:expr) => (
        IndexVec::from(vec![$elem; $n])
    );
    ($($x:expr),+ $(,)?) => (
        IndexVec::from(vec![$($x),+])
    );
}
pub(crate) use index_vec;

#[derive(Debug, Clone)]
pub struct Iter<'a, K: Idx, V> {
    values:  core::slice::Iter<'a, V>,
    current: K,
}

impl<'a, K: Idx, V> Iterator for Iter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|value| (self.current.next(), value))
    }
}

#[derive(Debug)]
pub struct IterMut<'a, K: Idx, V> {
    values:  core::slice::IterMut<'a, V>,
    current: K,
}

impl<'a, K: Idx, V> Iterator for IterMut<'a, K, V> {
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|value| (self.current.next(), value))
    }
}

#[derive(Debug)]
pub struct IntoIter<K: Idx, V> {
    values:  std::vec::IntoIter<V>,
    current: K,
}

impl<K: Idx, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|value| (self.current.next(), value))
    }
}
