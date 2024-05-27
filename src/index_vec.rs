use std::{
    marker::PhantomData,
    ops::{Range, RangeFrom},
};

pub trait Key: Copy {
    fn to_index(self) -> usize;
    fn from_index(index: usize) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexVec<K: Key, V> {
    values: Vec<V>,
    unused: PhantomData<K>,
}

impl<K: Key, V> IndexVec<K, V> {
    pub fn new() -> Self {
        Self {
            values: vec![],
            unused: PhantomData,
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            values: Vec::with_capacity(cap),
            unused: PhantomData,
        }
    }

    pub fn shrink_to_fit(&mut self) {
        self.values.shrink_to_fit();
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = V>) {
        self.values.extend(iter);
    }

    pub fn capacity(&self) -> usize {
        self.values.capacity()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn push(&mut self, value: V) {
        self.values.push(value);
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.values.insert(key.to_index(), value);
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.values.get(key.to_index())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.values.get_mut(key.to_index())
    }

    pub fn retain<F: FnMut(&V) -> bool>(&mut self, f: F) {
        self.values.retain(f)
    }

    pub fn retain_mut<F: FnMut(&mut V) -> bool>(&mut self, f: F) {
        self.values.retain_mut(f)
    }

    pub fn iter(&self) -> Iter<K, V> {
        Iter {
            values:  self.values.iter(),
            current: K::from_index(0),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        IterMut {
            values:  self.values.iter_mut(),
            current: K::from_index(0),
        }
    }

    pub fn inner(&self) -> &[V] {
        &self.values
    }

    pub fn indices(&self) -> impl Iterator<Item = K> {
        (0..self.values.len()).map(K::from_index)
    }

    pub fn inner_mut(&mut self) -> &mut Vec<V> {
        &mut self.values
    }

    pub fn into_inner(self) -> Vec<V> {
        self.values
    }

    pub fn first(&self) -> Option<&V> {
        self.values.first()
    }

    pub fn last(&self) -> Option<&V> {
        self.values.last()
    }

    pub fn first_mut(&mut self) -> Option<&mut V> {
        self.values.first_mut()
    }

    pub fn last_mut(&mut self) -> Option<&mut V> {
        self.values.last_mut()
    }
}

impl<K: Key, V> Default for IndexVec<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Key, V> IntoIterator for IndexVec<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            values:  self.values.into_iter(),
            current: K::from_index(0),
        }
    }
}

impl<'a, K: Key, V> IntoIterator for &'a IndexVec<K, V> {
    type Item = (K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            values:  self.values.iter(),
            current: K::from_index(0),
        }
    }
}

impl<'a, K: Key, V> IntoIterator for &'a mut IndexVec<K, V> {
    type Item = (K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            values:  self.values.iter_mut(),
            current: K::from_index(0),
        }
    }
}

impl<K: Key, V> From<Vec<V>> for IndexVec<K, V> {
    fn from(values: Vec<V>) -> Self {
        Self {
            values,
            unused: PhantomData,
        }
    }
}

impl<K: Key, V> core::ops::Index<K> for IndexVec<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.values[index.to_index()]
    }
}

impl<K: Key, V> core::ops::IndexMut<K> for IndexVec<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.values[index.to_index()]
    }
}

impl<K: Key, V> core::ops::Index<Range<K>> for IndexVec<K, V> {
    type Output = [V];

    fn index(&self, index: Range<K>) -> &Self::Output {
        &self.values[index.start.to_index()..index.end.to_index()]
    }
}

impl<K: Key, V> core::ops::IndexMut<Range<K>> for IndexVec<K, V> {
    fn index_mut(&mut self, index: Range<K>) -> &mut Self::Output {
        &mut self.values[index.start.to_index()..index.end.to_index()]
    }
}

impl<K: Key, V> core::ops::Index<RangeFrom<K>> for IndexVec<K, V> {
    type Output = [V];

    fn index(&self, index: RangeFrom<K>) -> &Self::Output {
        &self.values[index.start.to_index()..]
    }
}

impl<K: Key, V> core::ops::IndexMut<RangeFrom<K>> for IndexVec<K, V> {
    fn index_mut(&mut self, index: RangeFrom<K>) -> &mut Self::Output {
        &mut self.values[index.start.to_index()..]
    }
}

impl<'a, K: Key, V> From<&'a IndexVec<K, V>> for &'a [V] {
    fn from(index_vec: &'a IndexVec<K, V>) -> Self {
        index_vec.values.as_slice()
    }
}

impl<'a, K: Key, V> From<&'a mut IndexVec<K, V>> for &'a mut [V] {
    fn from(index_vec: &'a mut IndexVec<K, V>) -> Self {
        index_vec.values.as_mut_slice()
    }
}

impl<K: Key, V> FromIterator<(K, V)> for IndexVec<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let values = iter.into_iter().map(|(_, v)| v).collect();
        Self {
            values,
            unused: PhantomData,
        }
    }
}

impl<K: Key, V> FromIterator<V> for IndexVec<K, V> {
    fn from_iter<I: IntoIterator<Item = V>>(iter: I) -> Self {
        Self {
            values: Vec::from_iter(iter),
            unused: PhantomData,
        }
    }
}

impl Key for usize {
    fn to_index(self) -> usize {
        self
    }

    fn from_index(index: usize) -> Self {
        index
    }
}

impl Key for u32 {
    fn to_index(self) -> usize {
        self as usize
    }

    fn from_index(index: usize) -> Self {
        index as u32
    }
}

macro_rules! index_vec {
    () => (
        IndexVec::from(vec![])
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
pub struct Iter<'a, K: Key, V> {
    values:  std::slice::Iter<'a, V>,
    current: K,
}

impl<'a, K: Key, V> Iterator for Iter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|value| {
            let current = self.current;
            self.current = K::from_index(current.to_index() + 1);
            (current, value)
        })
    }
}

#[derive(Debug)]
pub struct IterMut<'a, K: Key, V> {
    values:  core::slice::IterMut<'a, V>,
    current: K,
}

impl<'a, K: Key, V> Iterator for IterMut<'a, K, V> {
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|value| {
            let current = self.current;
            self.current = K::from_index(current.to_index() + 1);
            (current, value)
        })
    }
}

#[derive(Debug)]
pub struct IntoIter<K: Key, V> {
    values:  std::vec::IntoIter<V>,
    current: K,
}

impl<K: Key, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|value| {
            let current = self.current;
            self.current = K::from_index(current.to_index() + 1);
            (current, value)
        })
    }
}
