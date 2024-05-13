use core::marker::PhantomData;

pub trait Key: Copy {
    fn to_index(self) -> usize;
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

    pub fn extend(&mut self, iter: impl IntoIterator<Item = V>) {
        self.values.extend(iter);
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn push(&mut self, value: V) {
        self.values.push(value);
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

    pub fn iter(&self) -> core::slice::Iter<V> {
        self.values.iter()
    }

    pub fn iter_mut(&mut self) -> core::slice::IterMut<V> {
        self.values.iter_mut()
    }

    pub fn inner(&self) -> &Vec<V> {
        &self.values
    }

    pub fn inner_mut(&mut self) -> &mut Vec<V> {
        &mut self.values
    }

    pub fn into_inner(self) -> Vec<V> {
        self.values
    }
}

impl<'a, K: Key, V> IntoIterator for IndexVec<K, V> {
    type Item = V;
    type IntoIter = std::vec::IntoIter<V>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
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
}

impl Key for u32 {
    fn to_index(self) -> usize {
        self as usize
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
