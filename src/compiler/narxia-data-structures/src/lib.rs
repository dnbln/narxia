use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::btree_map;
use std::collections::hash_map;
use std::fmt;
use std::hash::Hash;
use std::ops::Index;

pub struct FxHashMap<K: Hash + Eq, V>(HashMap<K, V>);

impl<K0: Hash + Eq + fmt::Debug, V0: fmt::Debug> fmt::Debug for FxHashMap<K0, V0> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <HashMap<K0, V0> as fmt::Debug>::fmt(&self.0, f)
    }
}

impl<K: Hash + Eq, V> Default for FxHashMap<K, V> {
    fn default() -> Self {
        Self(HashMap::default())
    }
}

impl<K: Hash + Eq, V: PartialEq> PartialEq for FxHashMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<K: Hash + Eq, V: Eq> Eq for FxHashMap<K, V> {}

impl<K: Hash + Eq + Clone, V: Clone> Clone for FxHashMap<K, V> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<K: Hash + Eq, V> FxHashMap<K, V> {
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get(k)
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.contains_key(k)
    }

    pub fn drain(&mut self) -> hash_map::Drain<'_, K, V> {
        self.0.drain()
    }

    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get_mut(k)
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.0.insert(k, v)
    }
}

impl<K, V> Index<K> for FxHashMap<K, V>
where
    K: Hash + Eq,
{
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        self.0.index(&index)
    }
}

pub struct FxHashSet<K>(HashSet<K>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FxBTreeMap<K: Ord, V>(BTreeMap<K, V>);

impl<K: Ord, V> Default for FxBTreeMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Ord, V> FxBTreeMap<K, V> {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.0.insert(k, v)
    }

    pub fn get<Q: Ord + ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.0.get(k)
    }

    pub fn iter(&self) -> btree_map::Iter<'_, K, V> {
        self.0.iter()
    }
}

impl<K: Ord, V> IntoIterator for FxBTreeMap<K, V> {
    type Item = (K, V);
    type IntoIter = btree_map::IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<K: Ord, KA: AsRef<K>, V> Index<KA> for FxBTreeMap<K, V> {
    type Output = V;

    fn index(&self, index: KA) -> &Self::Output {
        self.0.index(index.as_ref())
    }
}

pub struct FxBTreeSet<K>(BTreeSet<K>);
