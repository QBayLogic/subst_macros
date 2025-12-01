// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// Make a list of substitutions in the input in parallel and make a callback with the result
///
/// The caller is responsible for guarding this with a `const _: () = { ... }` if necessary to avoid
/// polluting the module namespace or overwriting an existing macro def. This macro will emit a
/// definition of `run_substitution` once per call.
///
/// Substitutions are given in two parts: a list of substitutions to make, and a scoped block in
/// which to perform the substitutions. Each of the items in the list of substitutions should look
/// like `[sub [TOKENS] = [TOKENS]]`. Any occurrence of the left side tokens will be replaced with
/// the right side tokens in the scoped block.
///
/// The callback may be given in two forms: either `NONE`, or a "standardized" callback block, which
/// has the following form:
/// ```txt
/// [
///     macro: path::to::macro, // note: do not put a `!` at the end
///     prefix: [PREFIX_TOKENS],
///     suffix: [SUFFIX_TOKENS],
/// ]
/// ```
/// In the case of a `NONE` callback, the result of the substitutions is output directly. In the
/// case that a callback is present, it is invoked as:
/// ```ignore
/// macro! {
///     PREFIX_TOKENS
///     SUBSTITUTION_OUTPUT
///     SUFFIX_TOKENS
/// }
/// ```
///
/// # Example
///
/// This example will expand to:
/// ```ignore
/// pub const MY_CONST: u8 = 42;
/// pub const TEST: u8 = MY_CONST + 42;
/// ```
/// ```
/// #![feature(macro_metavar_expr)]
///
/// # use subst_macros::parallel_subst;
///
/// parallel_subst! {
///     substitutions: [
///         [sub [FOO] = [MY_CONST]]
///         [sub [BAR] = [u8]]
///         [sub [BAZ BAQ] = [42]]
///     ],
///     callback: NONE,
///     in: {
///         pub const FOO: BAR = BAZ BAQ;
///         pub const TEST: BAR = FOO + BAZ BAQ;
///     }
/// }
///
/// fn main() {
///     assert_eq!(MY_CONST, 42u8);
///     assert_eq!(TEST, 84u8);
/// }
/// ```
#[macro_export]
macro_rules! parallel_subst {
    // Call this branch
    (
        substitutions: $substs:tt,
        callback: $callback:tt,
        in: {$($tokens:tt)*}
    ) => {
        $crate::parallel_subst! {
            @run
            substitutions: $substs,
            callback: $callback,
            $($tokens)*
        }
    };

    // Empty input shortcuts
    (
        @run
        substitutions: $substs:tt,
        callback: NONE,
    ) => {};
    (
        @run
        substitutions: $substs:tt,
        callback: [
            macro: $macro:path,
            prefix: [$($prefix:tt)*],
            suffix: [$($suffix:tt)*],
        ],
    ) => {
        $macro! {
            $($prefix)*
            $($suffix)*
        }
    };

    // No substitutions shortcuts
    (
        @run
        substitutions: [],
        callback: NONE,
        $($tokens:tt)*
    ) => {
        $($tokens)*
    };
    (
        @run
        substitutions: [],
        callback: [
            macro: $macro:path,
            prefix: [$($prefix:tt)*],
            suffix: [$($suffix:tt)*],
        ],
        $($tokens:tt)*
    ) => {
        $macro! {
            $($prefix)*
            $($tokens)*
            $($suffix)*
        }
    };

    // Actual work
    (
        @run
        substitutions: [$([sub [$($lhs:tt)+] = [$($rhs:tt)*]])+],
        callback: $callback:tt,
        $($tokens:tt)*
    ) => {
        macro_rules! run_substitution {
            // Make callbacks
            (
                input: [],
                acc: [$$($$acc:tt)*],
                callback: NONE,
            ) => {
                $$($$acc)*
            };
            (
                input: [],
                acc: [$$($$acc:tt)*],
                callback: [
                    macro: $$macro:path,
                    prefix: [$$($prefix:tt)*],
                    suffix: [$$($suffix:tt)*],
                ],
            ) => {
                $$macro! {
                    $$($$prefix)*
                    $$($$acc)*
                    $$($$suffix)*
                }
            };

            // Catch callbacks
            (
                @catch []
                input: $$input:tt,
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
                $$($$rest:tt)*
            ) => {
                run_substitution! {
                    input: $$input,
                    acc: [$$($$acc)* [$$($$rest)*]],
                    callback: $$callback,
                }
            };
            (
                @catch ()
                input: $$input:tt,
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
                $$($$rest:tt)*
            ) => {
                run_substitution! {
                    input: $$input,
                    acc: [$$($$acc)* ($$($$rest)*)],
                    callback: $$callback,
                }
            };
            (
                @catch {}
                input: $$input:tt,
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
                $$($$rest:tt)*
            ) => {
                run_substitution! {
                    input: $$input,
                    acc: [$$($$acc)* {$$($$rest)*}],
                    callback: $$callback,
                }
            };

            $(
                // Match LHS, produce RHS
                (
                    input: [$($lhs)+ $$($$rest:tt)*],
                    acc: [$$($$acc:tt)*],
                    callback: $$callback:tt,
                ) => {
                    run_substitution! {
                        input: [$$($$rest)*],
                        acc: [$$($$acc)* $($rhs)*],
                        callback: $$callback,
                    }
                };
            )+
            // Recurse into []
            (
                input: [[$$($$inner:tt)*] $$($$rest:tt)*],
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
            ) => {
                run_substitution! {
                    input: [$$($$inner)*],
                    acc: [],
                    callback: [
                        macro: run_substitution,
                        prefix: [
                            @catch []
                            input: [$$($$rest)*],
                            acc: [$$($$acc)*],
                            callback: $$callback,
                        ],
                        suffix: [],
                    ],
                }
            };
            // Recurse into ()
            (
                input: [($$($$inner:tt)*) $$($$rest:tt)*],
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
            ) => {
                run_substitution! {
                    input: [$$($$inner)*],
                    acc: [],
                    callback: [
                        macro: run_substitution,
                        prefix: [
                            @catch ()
                            input: [$$($$rest)*],
                            acc: [$$($$acc)*],
                            callback: $$callback,
                        ],
                        suffix: [],
                    ],
                }
            };
            // Recurse into {}
            (
                input: [{$$($$inner:tt)*} $$($$rest:tt)*],
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
            ) => {
                run_substitution! {
                    input: [$$($$inner)*],
                    acc: [],
                    callback: [
                        macro: run_substitution,
                        prefix: [
                            @catch {}
                            input: [$$($$rest)*],
                            acc: [$$($$acc)*],
                            callback: $$callback,
                        ],
                        suffix: [],
                    ],
                }
            };
            // Otherwise pass through unmatched tokens
            (
                input: [$$head:tt $$($$rest:tt)*],
                acc: [$$($$acc:tt)*],
                callback: $$callback:tt,
            ) => {
                run_substitution! {
                    input: [$$($$rest)*],
                    acc: [$$($$acc)* $$head],
                    callback: $$callback,
                }
            };
        }

        run_substitution! {
            input: [$($tokens)*],
            acc: [],
            callback: $callback,
        }
    };
}
