# Frunk Architecture

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                              FRUNK (v0.1.31)                                    │
│               Functional Programming Toolbelt for Rust                          │
└─────────────────────────────────────────────────────────────────────────────────┘
                                       │
                                       │
                    ┌──────────────────┼──────────────────┐
                    │                  │                  │
                    ▼                  ▼                  ▼
        ┌───────────────────┐ ┌──────────────┐  ┌──────────────────┐
        │   frunk_core      │ │ frunk_derives│  │   frunk_laws     │
        │   (v0.0.18)       │ │  (v0.0.19)   │  │   (v0.0.9)       │
        │                   │ │              │  │                  │
        │  Core building    │ │  Procedural  │  │  Property-based  │
        │  blocks           │ │  macros      │  │  testing         │
        └───────────────────┘ └──────────────┘  └──────────────────┘
                 │                    │                   │
                 │                    │                   │
     ┌───────────┼────────────┬───────┘                   │
     │           │            │                           │
     ▼           ▼            ▼                           ▼
┌─────────┐ ┌─────────┐ ┌──────────┐            ┌─────────────────┐
│ HList   │ │ Generic │ │Labelled  │            │ semigroup_laws  │
│         │ │         │ │          │            │ monoid_laws     │
└─────────┘ └─────────┘ └──────────┘            └─────────────────┘


┌─────────────────────────────────────────────────────────────────────────────────┐
│                         MODULE ORGANIZATION                                     │
└─────────────────────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────────────────┐
│  frunk/src/lib.rs                                                            │
├──────────────────────────────────────────────────────────────────────────────┤
│  ├── semigroup.rs    ← Things that can be combined                           │
│  ├── monoid.rs       ← Things with combine + identity                        │
│  ├── validated.rs    ← Accumulating Result (error collection)                │
│  ├── coproduct.rs    ← Ad-hoc sum types (type-safe unions)                   │
│  ├── kinder/         ← Higher-kinded type emulation                           │
│  └── Re-exports from frunk_core & frunk_derives                              │
└──────────────────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────────────────┐
│  frunk_core/src/lib.rs                                                       │
├──────────────────────────────────────────────────────────────────────────────┤
│  ├── hlist.rs        ← Heterogeneous lists (HCons, HNil)                     │
│  ├── generic.rs      ← Generic struct ↔ HList conversions                    │
│  └── labelled.rs     ← Type-level labels for named fields                    │
└──────────────────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────────────────┐
│  frunk_derives/src/lib.rs                                                    │
├──────────────────────────────────────────────────────────────────────────────┤
│  ├── derive_generic.rs          ← #[derive(Generic)]                         │
│  ├── derive_labelled_generic.rs ← #[derive(LabelledGeneric)]                 │
│  └── common.rs                  ← Shared derive utilities                    │
└──────────────────────────────────────────────────────────────────────────────┘


┌─────────────────────────────────────────────────────────────────────────────────┐
│                         CORE ABSTRACTIONS                                       │
└─────────────────────────────────────────────────────────────────────────────────┘

    ┌─────────────────────────────────────────────────────────────┐
    │                         HList                               │
    │  Heterogeneous, statically-typed lists                      │
    │                                                             │
    │  HCons<Head, Tail>  ──→  HCons<T2, ...>  ──→  HNil         │
    │    │                                                        │
    │    ├─ head: Head                                           │
    │    └─ tail: Tail                                           │
    │                                                             │
    │  Operations: pluck, sculpt, map, fold, reverse, etc.       │
    └─────────────────────────────────────────────────────────────┘
                             │
                             │ used by
                             ▼
    ┌─────────────────────────────────────────────────────────────┐
    │                       Generic                               │
    │  Convert structs ↔ HList                                    │
    │                                                             │
    │  struct Person { name, age }                                │
    │         │                │                                  │
    │         ▼                ▼                                  │
    │  HCons<String, HCons<i32, HNil>>                            │
    │                                                             │
    │  Methods: into_generic(), from_generic(), convert_from()   │
    └─────────────────────────────────────────────────────────────┘
                             │
                             │ enhanced by
                             ▼
    ┌─────────────────────────────────────────────────────────────┐
    │                  LabelledGeneric                            │
    │  Generic with type-level field labels                       │
    │                                                             │
    │  Field<"name", String> + Field<"age", i32>                  │
    │                                                             │
    │  Ensures field names match during conversions               │
    │  Methods: labelled_convert_from(), transform_from()         │
    └─────────────────────────────────────────────────────────────┘

    ┌─────────────────────────────────────────────────────────────┐
    │                      Coproduct                              │
    │  Type-safe sum types (A | B | C)                            │
    │                                                             │
    │  Inl(A) ──or──→ Inr(Coproduct<B, ...>)                     │
    │                                                             │
    │  Methods: inject(), get(), fold()                           │
    │  Used for: Ad-hoc unions of unrelated types                 │
    └─────────────────────────────────────────────────────────────┘

    ┌─────────────────────────────────────────────────────────────┐
    │                      Validated                              │
    │  Accumulating error handling                                │
    │                                                             │
    │  Result<A, E> + Result<B, E> + Result<C, E>                 │
    │         │           │              │                        │
    │         └───────────┴──────────────┘                        │
    │                     │                                       │
    │         ┌───────────┴──────────────┐                        │
    │         ▼                          ▼                        │
    │  Ok(HList![A,B,C])         Err(Vec<E>)                      │
    │                                                             │
    │  Collects ALL errors instead of short-circuiting           │
    └─────────────────────────────────────────────────────────────┘

    ┌─────────────────────────────────────────────────────────────┐
    │                  Semigroup/Monoid                           │
    │  Algebraic abstractions for combining values                │
    │                                                             │
    │  Semigroup:  combine(a, b) → c                              │
    │  Monoid:     Semigroup + empty() → identity                 │
    │                                                             │
    │  Implemented for: Option, Vec, String, numbers, tuples,     │
    │                   HLists, Product, Sum, All, Any            │
    └─────────────────────────────────────────────────────────────┘


┌─────────────────────────────────────────────────────────────────────────────────┐
│                         DATA FLOW EXAMPLE                                       │
└─────────────────────────────────────────────────────────────────────────────────┘

User Code:
    #[derive(Generic, LabelledGeneric)]
    struct ApiUser { FirstName, LastName, Age }
          │
          │ (via frunk_derives proc macro)
          ▼
    impl Generic for ApiUser {
        type Repr = HCons<String, HCons<String, HCons<i32, HNil>>>
        ...
    }
          │
          │ into_generic()
          ▼
    HList: HCons<String, HCons<String, HCons<i32, HNil>>>
          │
          │ pluck(), sculpt(), map(), etc.
          ▼
    Modified HList
          │
          │ from_generic()
          ▼
    struct DomainUser { first_name, last_name, age }


┌─────────────────────────────────────────────────────────────────────────────────┐
│                      DEPENDENCY GRAPH                                           │
└─────────────────────────────────────────────────────────────────────────────────┘

        frunk (main crate)
           │
           ├──[depends]──→ frunk_core (fundamental types)
           │                    │
           │                    ├─ HList (HCons/HNil)
           │                    ├─ Generic trait
           │                    └─ Labelled types
           │
           ├──[depends]──→ frunk_derives (proc macros)
           │                    │
           │                    ├─ #[derive(Generic)]
           │                    └─ #[derive(LabelledGeneric)]
           │
           └──[dev-dep]──→ frunk_laws (testing)
                              │
                              ├─ semigroup_laws
                              └─ monoid_laws

        User Crate
           │
           └──[depends]──→ frunk
                          (gets everything via re-exports)


┌─────────────────────────────────────────────────────────────────────────────────┐
│                         KEY DESIGN PATTERNS                                     │
└─────────────────────────────────────────────────────────────────────────────────┘

1. Type-Level Programming
   • HList length and structure encoded in types
   • Compile-time guarantees for operations

2. Generic Programming via Isomorphisms
   • Struct ≅ HList (Generic)
   • Enables abstracting over struct shapes

3. Accumulator Pattern
   • Validated collects all errors
   • Semigroup/Monoid for combining values

4. Zero-Cost Abstractions
   • All operations inline/optimize away
   • No runtime overhead

5. Trait-Based Extension
   • Traits define capabilities (Sculptor, Plucker, etc.)
   • Implemented recursively on HList structure
```
