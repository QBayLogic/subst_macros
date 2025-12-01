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
            dollar: [$],
            substitutions: $substs,
            callback: $callback,
            $($tokens)*
        }
    };

    // Empty input shortcuts
    (
        @run
        dollar: $dollar:tt,
        substitutions: $substs:tt,
        callback: NONE,
    ) => {};
    (
        @run
        dollar: $dollar:tt,
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
        dollar: $dollar:tt,
        substitutions: [],
        callback: NONE,
        $($tokens:tt)*
    ) => {
        $($tokens)*
    };
    (
        @run
        dollar: $dollar:tt,
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
        dollar: [$dollar:tt],
        substitutions: [$([sub [$($lhs:tt)+] = [$($rhs:tt)*]])+],
        callback: $callback:tt,
        $($tokens:tt)*
    ) => {
        macro_rules! run_substitution {
            // Make callbacks
            (
                input: [],
                acc: [$dollar($dollar acc:tt)*],
                callback: NONE,
            ) => {
                $dollar($dollar acc)*
            };
            (
                input: [],
                acc: [$dollar($dollar acc:tt)*],
                callback: [
                    macro: $dollar macro:path,
                    prefix: [$dollar($prefix:tt)*],
                    suffix: [$dollar($suffix:tt)*],
                ],
            ) => {
                $dollar macro! {
                    $dollar($dollar prefix)*
                    $dollar($dollar acc)*
                    $dollar($dollar suffix)*
                }
            };

            // Catch callbacks
            (
                @catch []
                input: $dollar input:tt,
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
                $dollar($dollar rest:tt)*
            ) => {
                run_substitution! {
                    input: $dollar input,
                    acc: [$dollar($dollar acc)* [$dollar($dollar rest)*]],
                    callback: $dollar callback,
                }
            };
            (
                @catch ()
                input: $dollar input:tt,
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
                $dollar($dollar rest:tt)*
            ) => {
                run_substitution! {
                    input: $dollar input,
                    acc: [$dollar($dollar acc)* ($dollar($dollar rest)*)],
                    callback: $dollar callback,
                }
            };
            (
                @catch {}
                input: $dollar input:tt,
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
                $dollar($dollar rest:tt)*
            ) => {
                run_substitution! {
                    input: $dollar input,
                    acc: [$dollar($dollar acc)* {$dollar($dollar rest)*}],
                    callback: $dollar callback,
                }
            };

            $(
                // Match LHS, produce RHS
                (
                    input: [$($lhs)+ $dollar($dollar rest:tt)*],
                    acc: [$dollar($dollar acc:tt)*],
                    callback: $dollar callback:tt,
                ) => {
                    run_substitution! {
                        input: [$dollar($dollar rest)*],
                        acc: [$dollar($dollar acc)* $($rhs)*],
                        callback: $dollar callback,
                    }
                };
            )+
            // Recurse into []
            (
                input: [[$dollar($dollar inner:tt)*] $dollar($dollar rest:tt)*],
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
            ) => {
                run_substitution! {
                    input: [$dollar($dollar inner)*],
                    acc: [],
                    callback: [
                        macro: run_substitution,
                        prefix: [
                            @catch []
                            input: [$dollar($dollar rest)*],
                            acc: [$dollar($dollar acc)*],
                            callback: $dollar callback,
                        ],
                        suffix: [],
                    ],
                }
            };
            // Recurse into ()
            (
                input: [($dollar($dollar inner:tt)*) $dollar($dollar rest:tt)*],
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
            ) => {
                run_substitution! {
                    input: [$dollar($dollar inner)*],
                    acc: [],
                    callback: [
                        macro: run_substitution,
                        prefix: [
                            @catch ()
                            input: [$dollar($dollar rest)*],
                            acc: [$dollar($dollar acc)*],
                            callback: $dollar callback,
                        ],
                        suffix: [],
                    ],
                }
            };
            // Recurse into {}
            (
                input: [{$dollar($dollar inner:tt)*} $dollar($dollar rest:tt)*],
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
            ) => {
                run_substitution! {
                    input: [$dollar($dollar inner)*],
                    acc: [],
                    callback: [
                        macro: run_substitution,
                        prefix: [
                            @catch {}
                            input: [$dollar($dollar rest)*],
                            acc: [$dollar($dollar acc)*],
                            callback: $dollar callback,
                        ],
                        suffix: [],
                    ],
                }
            };
            // Otherwise pass through unmatched tokens
            (
                input: [$dollar head:tt $dollar($dollar rest:tt)*],
                acc: [$dollar($dollar acc:tt)*],
                callback: $dollar callback:tt,
            ) => {
                run_substitution! {
                    input: [$dollar($dollar rest)*],
                    acc: [$dollar($dollar acc)* $dollar head],
                    callback: $dollar callback,
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
