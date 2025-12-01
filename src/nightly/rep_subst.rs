// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// Do repeated serial substitutions, callback done on each invocation
///
/// The caller is responsible for guarding calls to this macro in `const _: () = { ... };` if
/// necessary. This macro will emit one definition of `run_substitution!` for each `subst`itution
/// rule given.
///
/// # Example
/// ```
/// #![feature(macro_metavar_expr)]
/// #![recursion_limit = "398"]
///
/// struct Foo(u8);
///
/// subst_macros::repeat_serial_subst! {
///     groups: [
///         [group
///             [sub [GEN] = []]
///             [sub [LHSTY] = [TY]]
///             [sub [RHSTY] = [TY]]
///             [sub [SELFPRE] = []]
///             [sub [RHSPRE] = []]
///         ]
///         [group
///             [sub [GEN] = [<'a>]]
///             [sub [LHSTY] = [TY]]
///             [sub [RHSTY] = [&'a TY]]
///             [sub [SELFPRE] = []]
///             [sub [RHSPRE] = [&]]
///         ]
///         [group
///             [sub [GEN] = [<'a>]]
///             [sub [LHSTY] = [&'a TY]]
///             [sub [RHSTY] = [TY]]
///             [sub [SELFPRE] = [&]]
///             [sub [RHSPRE] = []]
///         ]
///         [group
///             [sub [GEN] = [<'a, 'b>]]
///             [sub [LHSTY] = [&'a TY]]
///             [sub [RHSTY] = [&'b TY]]
///             [sub [SELFPRE] = [&]]
///             [sub [RHSPRE] = [&]]
///         ]
///     ],
///     callback: [
///         macro: subst_macros::serial_subst,
///         prefix: [
///             @run
///             substitutions: [[sub [TY] = [Foo]]],
///             callback: NONE,
///         ],
///         suffix: [],
///     ],
///     in: {
///         impl GEN core::ops::Add<RHSTY> for LHSTY {
///             type Output = Foo;
///
///             fn add(self, rhs: RHSTY) -> Self::Output {
///                 Foo((SELFPRE self.0).add(RHSPRE rhs.0))
///             }
///         }
///     }
/// }
/// ```
#[macro_export]
macro_rules! repeat_serial_subst {
    (
        groups: [$([group $($subst:tt)*])+],
        callback: $callback:tt,
        in: $in:tt
    ) => {
        $(
            $crate::repeat_serial_subst! {
                @group
                group: [$($subst)*],
                callback: $callback,
                in: $in
            }
        )+
    };
    (
        @callback
        groups: $groups:tt,
        callback: $callback:tt,
        $($in:tt)*
    ) => {
        $crate::repeat_serial_subst! {
            groups: $groups,
            callback: $callback,
            in: { $($in)* }
        }
    };
    (
        @group
        group: [],
        callback: NONE,
        in: { $($in:tt)* }
    ) => {
        $($in)*
    };
    (
        @group
        group: [],
        callback: [
            macro: $macro:path,
            prefix: [$($prefix:tt)*],
            suffix: [$($suffix:tt)*],
        ],
        in: { $($in:tt)* }
    ) => {
        $macro! {
            $($prefix)*
            $($in)*
            $($suffix)*
        }
    };
    (
        @group
        group: [$($subst:tt)+],
        callback: $callback:tt,
        in: { $($in:tt)* }
    ) => {
        $crate::serial_subst! {
            @run
            substitutions: [$($subst)*],
            callback: $callback,
            $($in)*
        }
    };
}

/// Do repeated parallel substitutions, callback done on each invocation
///
/// Note on this the _significantly_ lower `recursion_limit` as compared to [`repeat_serial_subst!`].
///
/// The caller is responsible for guarding calls to this macro in `const _: () = { ... };` if
/// necessary. This macro will emit one definition of `run_substitution!` for each `group` given.
///
/// # Example
/// ```
/// #![feature(macro_metavar_expr)]
/// #![recursion_limit = "130"]
///
/// struct Foo(u8);
///
/// subst_macros::repeat_parallel_subst! {
///     groups: [
///         [group
///             [sub [GEN] = []]
///             [sub [LHSTY] = [TY]]
///             [sub [RHSTY] = [TY]]
///             [sub [SELFPRE] = []]
///             [sub [RHSPRE] = []]
///         ]
///         [group
///             [sub [GEN] = [<'a>]]
///             [sub [LHSTY] = [TY]]
///             [sub [RHSTY] = [&'a TY]]
///             [sub [SELFPRE] = []]
///             [sub [RHSPRE] = [&]]
///         ]
///         [group
///             [sub [GEN] = [<'a>]]
///             [sub [LHSTY] = [&'a TY]]
///             [sub [RHSTY] = [TY]]
///             [sub [SELFPRE] = [&]]
///             [sub [RHSPRE] = []]
///         ]
///         [group
///             [sub [GEN] = [<'a, 'b>]]
///             [sub [LHSTY] = [&'a TY]]
///             [sub [RHSTY] = [&'b TY]]
///             [sub [SELFPRE] = [&]]
///             [sub [RHSPRE] = [&]]
///         ]
///     ],
///     callback: [
///         macro: subst_macros::parallel_subst,
///         prefix: [
///             @run
///             substitutions: [[sub [TY] = [Foo]]],
///             callback: NONE,
///         ],
///         suffix: [],
///     ],
///     in: {
///         impl GEN core::ops::Add<RHSTY> for LHSTY {
///             type Output = Foo;
///
///             fn add(self, rhs: RHSTY) -> Self::Output {
///                 Foo((SELFPRE self.0).add(RHSPRE rhs.0))
///             }
///         }
///     }
/// }
/// ```
#[macro_export]
macro_rules! repeat_parallel_subst {
    (
        groups: [$([group $($subst:tt)*])+],
        callback: $callback:tt,
        in: $in:tt
    ) => {
        $(
            $crate::repeat_parallel_subst! {
                @group
                group: [$($subst)*],
                callback: $callback,
                in: $in
            }
        )+
    };
    (
        @callback
        groups: $groups:tt,
        callback: $callback:tt,
        $($in:tt)*
    ) => {
        $crate::repeat_parallel_subst! {
            groups: $groups,
            callback: $callback,
            in: { $($in)* }
        }
    };
    (
        @group
        group: [],
        callback: NONE,
        in: { $($in:tt)* }
    ) => {
        $($in)*
    };
    (
        @group
        group: [],
        callback: [
            macro: $macro:path,
            prefix: [$($prefix:tt)*],
            suffix: [$($suffix:tt)*],
        ],
        in: { $($in:tt)* }
    ) => {
        $macro! {
            $($prefix)*
            $($in)*
            $($suffix)*
        }
    };
    (
        @group
        group: [$($subst:tt)+],
        callback: $callback:tt,
        in: { $($in:tt)* }
    ) => {
        $crate::parallel_subst! {
            @run
            substitutions: [$($subst)*],
            callback: $callback,
            $($in)*
        }
    };
}
