# gencache

🚧 work in progress 🚧

Generational in-memory cache. This is a general-purpose cache library
for OCaml applications.

Goals:
- easy to use
- works well in all situations where a cache is useful

This cache lets the user specify the size of each entry (_size_) and the cost
of recomputing the entry (_cost_). The eviction policy uses the following
score:

> _priority_ = (_frequency_ / _size_) × (_cost_ / _size_)

where _frequency_ is the estimated recent access frequency of the cache entry.

This allows for an effective retention policy in these two common
models:

1. The cost of computing an entry is proportional to its size,
   e.g. ASTs obtained by parsing files of various sizes. In this case,
   the size of the file is a good proxy for the size of the AST for
   prioritization purposes. The recomputation cost is proportional
   to the file size as well.
2. The cost of obtaining an object is constant, possibly bound by network
   latency and independent from its size. In this case, _cost_ would
   be set to a constant instead of being set to the object size.
   Unless _size_ is also set to a constant, smaller objects are
   preferentially retained in the cache. When _size_ is set to a
   constant regardless of the actual object size, it may cause a memory
   blow-up.

To avoid certain catastrophic behavior due to the frequency estimates
being inaccurate for fresh entries, the cache uses two sub-caches:

- a minor cache for holding recent entries for which the
  access frequency is not fully established;
- a major cache for older entries that should not be evicted easily.

🚧 work in progress 🚧
