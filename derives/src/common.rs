use syn::{Ident, DeriveInput};
use proc_macro::TokenStream;
use quote::quote;
use proc_macro2::TokenStream as TokenStream2;

/// Parses a TokenStream (usually received as input into a
/// custom derive function), into a syn DeriveInput AST,
/// which is nice.
pub fn to_ast(input: TokenStream) -> DeriveInput {
    syn::parse(input).unwrap()
}

/// Given a identifiers, creates an AST for building an HList (HCons)
/// using those identifiers as accessors.
///
/// Subsequently, this same function can be used for pattern matching too!
pub fn build_hcons_constr(accessors: &Vec<Ident>) -> TokenStream2 {
    match accessors.len() {
        0 => quote! { ::frunk_core::hlist::HNil },
        1 => {
            let h = &accessors[0];
            quote! { ::frunk_core::hlist::HCons{ head: #h, tail: ::frunk_core::hlist::HNil } }
        },
        _ => {
            let h = &accessors[0];
            let tail = accessors[1..].to_vec();
            let hlist_tail = build_hcons_constr(&tail);
            quote! { ::frunk_core::hlist::HCons{ head: #h, tail: #hlist_tail }}
        }
    }
}
