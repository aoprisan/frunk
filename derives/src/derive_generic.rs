use crate::common::{build_hcons_constr, to_ast};
use syn::{Ident, Data, Fields, Field, Type};
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::quote;

/// Given an AST, returns an implementation of Generic using HList
///
/// Only works with Structs and Tuple Structs
pub fn impl_generic(input: TokenStream) -> TokenStream2 {
    let ast = to_ast(input);
    let name = &ast.ident;
    let generics = &ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let fields: Vec<&Field> = match &ast.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named.named.iter().collect(),
            Fields::Unnamed(fields_unnamed) => fields_unnamed.unnamed.iter().collect(),
            Fields::Unit => vec![],
        },
        _ => panic!("Only structs are supported")
    };
    let field_types: Vec<&Type> = fields.iter()
        .map(|f| &f.ty).collect();
    let repr_type = build_repr(&field_types);
    let maybe_fnames: Vec<Option<&Ident>> = fields
        .iter()
        .map(|f| f.ident.as_ref())
        .collect();
    let is_tuple_struct = maybe_fnames.iter().all(|m_f| m_f.is_none());

    let fnames: Vec<Ident> = fields
        .iter()
        .enumerate()
        .map(|(i, f)| f.ident.clone().unwrap_or_else(|| Ident::new(&format!("_{}", i), Span::call_site())))
        .collect();
    let hcons_constr = build_hcons_constr(&fnames);
    let hcons_pat = build_hcons_constr(&fnames);
    let new_struct_constr = build_new_struct_constr(name, &fnames, is_tuple_struct);

    let struct_deconstr = if is_tuple_struct {
        quote! { #name ( #(#fnames, )* ) }
    } else {
        quote! { #name { #(#fnames, )* } }
    };

    quote! {
        #[allow(non_snake_case, non_camel_case_types)]
        impl #impl_generics ::frunk_core::generic::Generic for #name #ty_generics #where_clause {

            type Repr = #repr_type;

            fn into(self) -> Self::Repr {
                let #struct_deconstr = self;
                #hcons_constr
            }

            fn from(r: Self::Repr) -> Self {
                let #hcons_pat = r;
                #new_struct_constr
            }
        }
    }
}

fn build_repr(field_types: &Vec<&Type>) -> TokenStream2 {
    match field_types.len() {
        0 => quote! { ::frunk_core::hlist::HNil },
        1 => {
            let h = field_types[0];
            quote! { ::frunk_core::hlist::HCons<#h, ::frunk_core::hlist::HNil> }
        },
        _ => {
            let h = field_types[0];
            let tail = field_types[1..].to_vec();
            let tail_type = build_repr(&tail);
            quote! { ::frunk_core::hlist::HCons<#h, #tail_type> }
        }
    }
}


fn build_new_struct_constr(struct_name: &Ident, bindnames: &Vec<Ident>, is_tuple_struct: bool) -> TokenStream2 {
    if is_tuple_struct {
        quote! { #struct_name (#(#bindnames),* ) }
    } else {
        quote! { #struct_name { #(#bindnames: #bindnames),* } }
    }
}
