// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![cfg_attr(feature = "macro_metavar_expr", feature(macro_metavar_expr))]
#![no_std]

//! Perform substitutions using declarative macros (with optional callbacks).
//!
//! # Why would I want this?
//!
//! This is a niche application, even by the author's admission. The use case that brought these
//! macros into being was defining new "primitive" types, where it was desired to share
//! nearly-identical bits of code across implementations, such as `where` clauses or function
//! bodies. For instance, in a macro invocation you might provide a `where LHSTY: NEWTRAIT<RHSTY>`
//! above a list of traits to implement for inner types for a newtype-shaped struct:
//! ```ignore
//! struct Foo<T>(T);
//!
//! trait HasInnerTy {
//!     type Inner;
//! }
//!
//! type GetInner<T> = <T as HasInnerTy>::Inner;
//!
//! define_and_impl_traits_on_primitives! {
//!     newtype: Foo
//!     where: [LHSINNER: NEWTRAIT<RHSINNER, Output = INNER>],
//!     ops: [
//!         MarkerAdd::marker_add = Add::add,
//!         MarkerSub::marker_sub = Sub::sub,
//!         // ...
//!     ],
//!     primitives: [u8, u16, u32, u64, u128, i8, i16, i32, i64, i128]
//! }
//!
//! // Imagine that the above gives the following `impl`s on `Foo` (among others):
//! impl<T> Add<Foo<T>> for Foo<T>
//! where
//!     T: MarkerAdd<T, Output = T>,
//! {
//!     type Output = Foo<T>;
//!
//!     fn add(self, rhs: Foo<T>) -> Self::Output {
//!         Foo(self.0.marker_add(rhs.0))
//!     }
//! }
//!
//! impl<'a, T> Add<&'a Foo<T>> for Foo<T>
//! where
//!     T: MarkerAdd<&'a T, Output = T>,
//! {
//!     type Output = Foo<T>;
//!
//!     fn add(self, rhs: &'a Foo<T>) -> Self::Output {
//!         Foo(self.0.marker_add(&rhs.0))
//!     }
//! }
//!
//! impl<'a, T> Add<Foo<T>> for &'a Foo<T>
//! where
//!     &'a T: MarkerAdd<T, Output = T>,
//! {
//!     type Output = Foo<T>;
//!
//!     fn add(self, rhs: Foo<T>) -> Self::Output {
//!         Foo((&self.0).marker_add(rhs.0))
//!     }
//! }
//!
//! // ...
//!
//! impl<T> Sub<Foo<T>> for Foo<T>
//! where
//!     T: MarkerSub<T, Output = T>,
//! {
//!     type Output = Foo<T>;
//!
//!     fn sub(self, rhs: Foo<T>) -> Self::Output {
//!         Foo(self.0.marker_sub(rhs.0))
//!     }
//! }
//!
//! // Note how in the above impls, the same shape of `where` clause is used as provided in the
//! // macro invocation, but some of its tokens have been replaced.
//!
//! // Now to be able to do `Foo<NonPrimitiveType> + Foo<NonPrimitiveType>` we just need to...
//! impl MarkerAdd for NonPrimitiveType {
//!     // ...
//! }
//! ```
//! The key idea here is that a macro caller can be allowed to provide a section of code that is
//! written to be generic over the _syntax_ of what it will be placed into eventually, by way of the
//! macro author providing tokens that will be substituted eventually.
//!
//! # Behaviour
//!
//! The exact behaviour changes per-macro, but the broad strokes are as follows:
//! - A `[sub ...]` corresponds to the definition of a macro branch which matches on the left-hand
//!   side tokens and produces the right-hand side tokens.
//! - All macros recurse into `[]`, `()`, and `{}` token trees.
//!
//! # `macro_metavar_expr` feature
//!
//! This feature requires a nightly compiler, since it enables `#![feature(macro_metavar_expr)]`,
//! and additionally requires the crate that calls these macros to .
//! Enabling this feature removes the necessity for the `$dollar:tt` input, which should only show
//! up if you're invoking macros as callbacks. However, it still is a nice quality-of-life upgrade
//! if you're already on nightly and have some more advanced usage of this crate's macros.
//!
//! # Repeated substitutions
//!
//! These macros are a very simple wrapper around [`serial_subst!`] and [`parallel_subst!`]. The
//! corresponding macro is called with each group of substitutions over the same input.
//!
//! # Common conventions
//!
//! There are two common conventions maintained throughout this crate: the format of callbacks and
//! substitutions.
//!
//! ## Callback convention
//!
//! Callbacks may be in one of two forms:
//! ```ignore
//! foo! {
//!     // ...
//!     callback: NONE,
//!     // ...
//! }
//!
//! foo! {
//!     // ...
//!     callback: [
//!         macro: path::to::macro, // NOTE: no `!`
//!         prefix: [asdf], // matched as: `[$($prefix:tt)*]`
//!         suffix: [qwerty], // matched as: `[$($suffix:tt)*]`
//!     ],
//!     // ...
//! }
//! ```
//! In the case that a macro in this crate has a callback and is invoked with one, then any "output"
//! tokens will go between the prefix and suffix tokens. For instance, if a macro expands to the
//! tokens `2 + 3`, and the callback
//! ```ignore
//! callback: [
//!     macro: println,
//!     prefix: ["{}", 1 + ],
//!     suffix: [+ 4],
//! ]
//! ```
//! is given, then that will expand to
//! ```ignore
//! println!(
//!     "{}", 1 +
//!     2 + 3
//!     + 4
//! )
//! ```
//!
//! ## Substitution convention
//!
//! Substitutions are _always_ matched as the following:
//! ```ignore
//! [sub [$($lhs:tt)+] = [$($rhs:tt)*]]
//! ```
//! That is to say, the left-hand side must be a group of arbitrary tokens with a minimum size of 1,
//! and the right-hand side must be a group of arbitrary tokens with a minimum size of 0.
//!
//! ## Advanced hackery
//!
//! What's very, very interesting is that it is _absolutely_ possible to use fragments in
//! substitution rules. If you have the `macro_metavar_expr` feature then you can write
//! substitutions like `[sub [[$$($$flat:tt)+]] = [$$($$flat)+]]`. Without that feature it
//! will be necessary to write it as
//! `[sub [[$dollar($dollar flat:tt)+]] = [$dollar($dollar flat)+]]`. This should then
//! flatten expressions such as `[[a b] [[c] [d e]]]` to `a b [c] [d e]`.
//!
//! # When do I need serial or parallel substitutions?
//!
//! You will need to do serial substitutions if your desired behaviour is that the output of one
//! substitution be matched and replaced by a following substitution. For instance:
//! ```
#![cfg_attr(
    feature = "macro_metavar_expr",
    doc = "#![feature(macro_metavar_expr)]\n\n"
)]
//! const BAR: u8 = 10;
//! const BAZ: u8 = 20;
//!
//! subst_macros::serial_subst! {
//!     substitutions: [
//!         [sub [FOO] = [BAR]]
//!         [sub [BAR] = [BAZ]]
//!     ],
//!     callback: NONE,
//!     in: {
//!         const BAQ: u8 = FOO;
//!     }
//! }
//!
//! // The above expands to:
//! // const BAQ: u8 = BAZ;
//!
//! fn main() {
//!     assert_eq!(BAQ, BAZ);
//! }
//! ```
//! However, with parallel substitutions:
//! ```
#![cfg_attr(
    feature = "macro_metavar_expr",
    doc = "#![feature(macro_metavar_expr)]\n\n"
)]
//! const BAR: u8 = 10;
//! const BAZ: u8 = 20;
//!
//! subst_macros::parallel_subst! {
//!     substitutions: [
//!         [sub [FOO] = [BAR]]
//!         [sub [BAR] = [BAZ]]
//!     ],
//!     callback: NONE,
//!     in: {
//!         const BAQ: u8 = FOO;
//!     }
//! }
//!
//! // The above expands to:
//! // const BAQ: u8 = BAR;
//!
//! fn main() {
//!     assert_eq!(BAQ, BAR);
//! }
//! ```
//! The difference being that in serial mode, `FOO` is first replaced with `BAR` and then `BAR` is
//! replaced with `BAZ`, but in parallel mode only the first step occurs, since that's the only rule
//! in the newly defined macro that matches the input.
//!
//! # How does it work?
//!
//! ## Serial substitutions
//!
//! In the following, if you have the `macro_metavar_expr` feature enabled you can just imagine that
//! `dollar: [$]` isn't there.
//!
//! For each `[sub ...]` in the input list, a helper macro is generated. This helper macro has
//! branches to do the following:
//! - Perform a `NONE` callback
//! - Perform a macro callback
//! - Catch `[]`, `()`, and `{}` recursion callbacks
//! - Look for the tokens to replace and then replace them
//! - Initiate `[]`, `()`, and `{}` recursion
//! - Pass through unmatched input tokens to the output
//!
//! Let's look at how one of these would look (with elision for brevity). Let's say we write
//! ```
#![cfg_attr(
    feature = "macro_metavar_expr",
    doc = "#![feature(macro_metavar_expr)]\n\n"
)]
//! subst_macros::serial_subst! {
//!     substitutions: [
//!         [sub [VALUE] = ["hello world!"]]
//!     ],
//!     callback: NONE,
//!     in: {
//!         println!(VALUE)
//!     }
//! }
//! ```
//! This will then expand into a new macro, plus an invocation of that new macro:
//! ```ignore
//! macro_rules! run_substitution {
//!     // elided branch: NONE callback
//!     // elided branch: macro callback
//!     // elided branch: catch [] recursion callback
//!     // elided branch: catch () recursion callback
//!     // elided branch: catch {} recursion callback
//!
//!     // Match LHS
//!     (
//!         input: [VALUE $($rest:tt)*], // NOTE: left hand side of subst consumed here
//!         acc: [$($acc:tt)*],
//!         callback: $callback:tt,
//!     ) => {
//!         run_substitution! {
//!             input: [$($rest)*],
//!             acc: [$($acc)* "hello world!"], // NOTE: right hand side of subst produced here
//!             callback: $callback,
//!         }
//!     };
//!     // elided branch: initiate [] recursion
//!     // elided branch: initiate () recursion
//!     // elided branch: initiate {} recursion
//!     // Otherwise pass through unmatched tokens
//!     (
//!         input: [$head:tt $($rest:tt)*],
//!         acc: [$($acc:tt)*],
//!         callback: $callback:tt,
//!     ) => {
//!         run_substitution! {
//!             input: [$($rest)*],
//!             acc: [$($acc)* $head],
//!             callback: $callback,
//!         }
//!     };
//! }
//!
//! run_substitution! {
//!     input: [println!(VALUE)],
//!     acc: [],
//!     callback: [
//!         macro: serial_subst,
//!         prefix: [
//!             @run
//!             dollar: [$],
//!             substitutions: [],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//! }
//! ```
//! We can then run through how this expands:
//! ```ignore
//! run_substitution! {
//!     input: [println!(VALUE)],
//!     acc: [],
//!     callback: [
//!         macro: serial_subst,
//!         prefix: [
//!             @run
//!             dollar: [$],
//!             substitutions: [],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//! }
//!
//! run_substitution! {
//!     input: [!(VALUE)],
//!     acc: [println],
//!     callback: [
//!         macro: serial_subst,
//!         prefix: [
//!             @run
//!             dollar: [$],
//!             substitutions: [],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//! }
//!
//! run_substitution! {
//!     input: [(VALUE)],
//!     acc: [println!],
//!     callback: [
//!         macro: serial_subst,
//!         prefix: [
//!             @run
//!             dollar: [$],
//!             substitutions: [],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//! }
//!
//! run_substitution! {
//!     input: [VALUE],
//!     acc: [],
//!     callback: [
//!         macro: run_substitution
//!         prefix: [
//!             input: [],
//!             acc: [println!],
//!             callback: [
//!                 macro: serial_subst,
//!                 prefix: [
//!                     @run
//!                     dollar: [$],
//!                     substitutions: [],
//!                     callback: NONE,
//!                 ],
//!                 suffix: [],
//!             ],
//!         ],
//!         suffix: [],
//!     ],
//! }
//!
//! run_substitution! {
//!     input: [],
//!     acc: ["hello world!"],
//!     callback: [
//!         macro: run_substitution
//!         prefix: [
//!             @catch ()
//!             input: [],
//!             acc: [println!],
//!             callback: [
//!                 macro: serial_subst,
//!                 prefix: [
//!                     @run
//!                     dollar: [$],
//!                     substitutions: [],
//!                     callback: NONE,
//!                 ],
//!                 suffix: [],
//!             ],
//!         ],
//!         suffix: [],
//!     ],
//! }
//!
//! run_substitution! {
//!     @catch ()
//!     input: [],
//!     acc: [println!],
//!     callback: [
//!         macro: serial_subst,
//!         prefix: [
//!             @run
//!             dollar: [$],
//!             substitutions: [],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//!     "hello world!"
//! }
//!
//! run_substitution! {
//!     input: [],
//!     acc: [println!("hello world!")],
//!     callback: [
//!         macro: serial_subst,
//!         prefix: [
//!             @run
//!             dollar: [$],
//!             substitutions: [],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//! }
//!
//! serial_subst! {
//!     @run
//!     dollar: [$],
//!     substitutions: [],
//!     callback: NONE,
//!     println!("hello world!")
//! }
//!
//! println!("hello world!")
//! ```
//! Note here that since the list of substitutions is empty, `serial_subst!` handles its callback.
//! However, if there was another substitution to do, then another macro would be defined and then
//! called in a similar manner to what is demonstrated above.
//!
//! ## Parallel substitutions
//!
//! In the following, if you have the `macro_metavar_expr` feature enabled you can just imagine that
//! `dollar: [$]` isn't there.
//!
//! For each `[sub ...]` in the input list, a rule is generated in the helper macro. The helper
//! macro has additional branches to do the following:
//! - Perform a `NONE` callback
//! - Perform a macro callback
//! - Catch `[]`, `()`, and `{}` recursion callbacks
//! - Look for the tokens to replace and then replace them
//! - Initiate `[]`, `()`, and `{}` recursion
//! - Pass through unmatched input tokens to the output
//!
//! Let's look at how one of these would look (with elision for brevity). Let's say we write
//! ```
#![cfg_attr(
    feature = "macro_metavar_expr",
    doc = "#![feature(macro_metavar_expr)]\n\n"
)]
//! subst_macros::parallel_subst! {
//!     substitutions: [
//!         [sub [FOO] = [MY_CONST]]
//!         [sub [BAR] = [u8]]
//!         [sub [BAZ BAQ] = [42]]
//!     ],
//!     callback: NONE,
//!     in: {
//!         pub const FOO: BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     }
//! }
//! ```
//! This will then expand into a new macro, plus an invocation of that new macro:
//! ```ignore
//! macro_rules! run_substitution {
//!     // elided branch: NONE callback
//!     // elided branch: macro callback
//!     // elided branch: catch [] recursion callback
//!     // elided branch: catch () recursion callback
//!     // elided branch: catch {} recursion callback
//!
//!     // Match LHS, produce RHS
//!     (
//!         input: [FOO $($rest:tt)*], // NOTE: left hand side of subst consumed here
//!         acc: [$($acc:tt)*],
//!         callback: $callback:tt,
//!     ) => {
//!         run_substitution! {
//!             input: [$($rest)*],
//!             acc: [$($acc)* MY_CONST], // NOTE: right hand side of subst produced here
//!             callback: $callback,
//!         }
//!     };
//!     // Match LHS, produce RHS
//!     (
//!         input: [BAR $($rest:tt)*], // NOTE: left hand side of subst consumed here
//!         acc: [$($acc:tt)*],
//!         callback: $callback:tt,
//!     ) => {
//!         run_substitution! {
//!             input: [$($rest)*],
//!             acc: [$($acc)* u8], // NOTE: right hand side of subst produced here
//!             callback: $callback,
//!         }
//!     };
//!     // Match LHS, produce RHS
//!     (
//!         input: [BAZ BAQ $($rest:tt)*], // NOTE: left hand side of subst consumed here
//!         acc: [$($acc:tt)*],
//!         callback: $callback:tt,
//!     ) => {
//!         run_substitution! {
//!             input: [$($rest)*],
//!             acc: [$($acc)* 42], // NOTE: right hand side of subst produced here
//!             callback: $callback,
//!         }
//!     };
//!     // elided branch: initiate [] recursion
//!     // elided branch: initiate () recursion
//!     // elided branch: initiate {} recursion
//!     // Otherwise pass through unmatched tokens
//!     (
//!         input: [$head:tt $($rest:tt)*],
//!         acc: [$($acc:tt)*],
//!         callback: $callback:tt,
//!     ) => {
//!         run_substitution! {
//!             input: [$($rest)*],
//!             acc: [$($acc)* $head],
//!             callback: $callback,
//!         }
//!     };
//! }
//!
//! run_substitution! {
//!     input: [
//!         pub const FOO: BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [],
//!     callback: NONE,
//! }
//! ```
//! We can then run through how this expands:
//! ```ignore
//! run_substitution! {
//!     input: [
//!         pub const FOO: BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         const FOO: BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         FOO: BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub const],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         : BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub const MY_CONST],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         BAR = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub const MY_CONST:],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         = BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub const MY_CONST: u8],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         BAZ BAQ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub const MY_CONST: u8 =],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [
//!         ;
//!         pub const TEST: BAR = FOO + BAZ BAQ;
//!     ],
//!     acc: [pub const MY_CONST: u8 = 42],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [pub const TEST: BAR = FOO + BAZ BAQ;],
//!     acc: [pub const MY_CONST: u8 = 42;],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [const TEST: BAR = FOO + BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [TEST: BAR = FOO + BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [: BAR = FOO + BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [BAR = FOO + BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST:
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [= FOO + BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST: u8
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [FOO + BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST: u8 =
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [+ BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST: u8 = MY_CONST
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [BAZ BAQ;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST: u8 = MY_CONST +
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [;],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST: u8 = MY_CONST + 42
//!     ],
//!     callback: NONE,
//! }
//!
//! run_substitution! {
//!     input: [],
//!     acc: [
//!         pub const MY_CONST: u8 = 42;
//!         pub const TEST: u8 = MY_CONST + 42;
//!     ],
//!     callback: NONE,
//! }
//!
//! pub const MY_CONST: u8 = 42;
//! pub const TEST: u8 = MY_CONST + 42;
//! ```
//!
//! # Best practices
//!
//! Using these macros can lead to very high `#![recursion_limit = "..."]` requirements very
//! quickly, so it's important to minimise this where possible. The best way to do this is by doing
//! parallel substitutions where possible, and then doing serial substitutions only where necessary.
//! Note that doing a parallel substitution with a callback to another parallel substitution is, in
//! effect, a serial operation. However, it is by far the better way to deal with dependencies
//! between substitutions.
//!
//! Specifically, what's meant by a parallel subst with a callback to another parallel subst being
//! equivalent to a serial subst is that these two are equivalent:
//! ```ignore
//! subst_macros::parallel_subst! {
//!     substitutions: [[sub [FOO] = [BAR]]],
//!     callback: [
//!         macro: subst_macros::parallel_subst,
//!         prefix: [
//!             substitutions: [[sub [BAR] = [BAZ]]],
//!             callback: NONE,
//!         ],
//!         suffix: [],
//!     ],
//!     in: {
//!         // ...
//!     }
//! }
//! ```
//! Is equivalent to
//! ```ignore
//! subst_macros::serial_subst! {
//!     substitutions: [
//!         [sub [FOO] = [BAR]]
//!         [SUBST [BAR] = [BAZ]]
//!     ],
//!     callback: NONE,
//!     in: {
//!         // ...
//!     }
//! }
//! ```
//! Except that in the former you're able to do multiple substitutions at the same time as
//! `[sub [FOO] = [BAR]]`, and before `[sub [BAR] = [BAZ]]`.

#[cfg(feature = "macro_metavar_expr")]
mod nightly;

#[cfg(not(feature = "macro_metavar_expr"))]
mod stable;
