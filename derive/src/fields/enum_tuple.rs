use super::{ParsedEnumVariant, ParsedField};
use crate::common::Exists;
use darling::{FromField, FromVariant};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::abort;
use quote::{format_ident, quote};
use syn::spanned::Spanned;

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct EnumTupleField {
	index: u32,
	field: ParsedField,
}

impl Exists for EnumTupleField {
	fn start_revision(&self) -> u16 {
		self.field.start.unwrap_or(0)
	}
	fn end_revision(&self) -> Option<u16> {
		self.field.end
	}
	fn sub_revision(&self) -> u16 {
		0
	}
}

impl EnumTupleField {
	pub fn check_attributes(&self, current: u16) {
		if !self.exists_at(current) && self.field.convert_fn.is_none() {
			abort!(
				self.field.ty.span(),
				"Expected a 'convert_fn' to be specified for field {}",
				self.index
			);
		}
	}

	pub fn generate_deserializer_field(
		&self,
		idx: u32,
		current: u16,
		revision: u16,
	) -> (TokenStream, Option<TokenStream>) {
		let kind = &self.field.ty;
		let field = format_ident!("v{}", idx);

		if !self.exists_at(revision) {
			if let Some(def) = self.field.default_fn.as_ref() {
				let def_fn = format_ident!("{}", def);
				return (
					quote! {
						let #field = Self::#def_fn();
					},
					None,
				);
			}

			if self.end_revision().map(|x| x <= revision).unwrap_or(false) {
				return (TokenStream::new(), None);
			} else {
				return (
					quote! {
						let #field = Default::default();
					},
					None,
				);
			}
		}

		if self.exists_at(revision) && !self.exists_at(current) {
			let convert_fn = self.field.convert_fn.clone().unwrap();
			let convert_fn = format_ident!("{convert_fn}");

			return (
				quote! {
					let #field = <#kind as revision::Revisioned>::deserialize_revisioned(reader)?;
				},
				Some(quote! {
					object.#convert_fn(revision, #field)?;
				}),
			);
		}

		(
			quote! {
				let #field = <#kind as revision::Revisioned>::deserialize_revisioned(reader)?;
			},
			None,
		)
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct EnumTuple {
	revision: u16,
	index: u32,
	fields: Vec<EnumTupleField>,
	is_unit: bool,
	parsed: ParsedEnumVariant,
}

impl Exists for EnumTuple {
	fn start_revision(&self) -> u16 {
		self.parsed.start.unwrap_or(self.revision)
	}
	fn end_revision(&self) -> Option<u16> {
		self.parsed.end
	}
	fn sub_revision(&self) -> u16 {
		self.fields
			.iter()
			.map(|x| x.end_revision().unwrap_or(1).max(x.start_revision()))
			.max()
			.unwrap_or(0)
	}
}

impl EnumTuple {
	pub fn new(revision: u16, variant: &syn::Variant, index: u32) -> Self {
		// Parse the variant macro attributes
		let parsed = match ParsedEnumVariant::from_variant(variant) {
			Ok(x) => x,
			Err(e) => {
				abort!(variant.ident.span(), "{}", e);
			}
		};

		let mut is_unit = false;

		// Process the enum variant fields
		let fields = match &variant.fields {
			syn::Fields::Unnamed(fields) => {
				let mut res = Vec::new();
				for (idx, field) in fields.unnamed.iter().enumerate() {
					match ParsedField::from_field(field) {
						Ok(x) => res.push(EnumTupleField {
							index: idx.try_into().unwrap(),
							field: x,
						}),
						Err(e) => {
							abort!(e.span(), "{e}")
						}
					}
				}
				res
			}
			syn::Fields::Unit => {
				is_unit = true;
				Vec::new()
			}
			_ => Vec::new(),
		};
		// Create the enum variant holder
		EnumTuple {
			revision,
			index,
			fields,
			parsed,
			is_unit,
		}
	}

	pub fn reexpand(&self, current: u16) -> TokenStream {
		let ident = &self.parsed.ident;
		let attrs = &self.parsed.attrs;
		if self.is_unit {
			quote!(
				#(#attrs)*
				#ident
			)
		} else {
			let fields = self.fields.iter().filter(|x| x.exists_at(current)).map(|x| &x.field.ty);
			quote!(
				#(#attrs)*
				#ident( #(#fields,)* )
			)
		}
	}

	pub fn check_attributes(&self, current: u16) {
		if !self.exists_at(current) && self.parsed.convert_fn.is_none() {
			abort!(
				self.parsed.ident.span(),
				"Expected a 'convert_fn' to be specified for enum variant {}",
				self.parsed.ident
			);
		}
	}

	pub fn generate_serializer(&self, current: u16) -> TokenStream {
		// Get the variant index
		let index = self.index;
		// Get the variant identifier
		let ident = &self.parsed.ident;
		// Create a token stream for the serializer
		let mut serializer = TokenStream::new();
		// Create a token stream for the variant fields
		let mut inner = TokenStream::new();
		// Loop over each of the enum variant fields
		for (index, _) in self.fields.iter().filter(|x| x.exists_at(current)).enumerate() {
			// Get the field identifier
			let field = format_ident!("v{}", index);
			// Extend the enum constructor
			inner.extend(quote!(#field,));
			// Extend the serializer
			serializer.extend(quote! {
				revision::Revisioned::serialize_revisioned(#field, writer)?;
			});
		}
		// Output the token stream
		if self.fields.is_empty() {
			if !self.exists_at(current) {
				panic!("tried to generate a serializer a field which was deleted.");
			} else {
				quote! {
					Self::#ident => {
						revision::Revisioned::serialize_revisioned(&#index, writer)?;
					},
				}
			}
		} else if !self.exists_at(current) {
			panic!("tried to generate a serializer a field which was deleted.");
		} else {
			quote! {
				Self::#ident(#inner) => {
					revision::Revisioned::serialize_revisioned(&#index, writer)?;
					#serializer
				},
			}
		}
	}

	pub fn generate_deserializer(&self, current: u16, revision: u16) -> TokenStream {
		// Get the variant index
		let index = self.index;
		// Get the variant identifier
		let ident = &self.parsed.ident;
		// Check if the variant is new.
		if !self.exists_at(revision) {
			return quote!();
		}
		// Create a token stream for the field deserialisation
		let mut deserializer = TokenStream::new();
		// Create a token stream for the fields
		let mut inner = TokenStream::new();

		let mut post_process = TokenStream::new();
		// Loop over the enum variant fields
		for (index, field) in self.fields.iter().enumerate() {
			field.check_attributes(current);
			// Get the field identifier
			let field_ident = format_ident!("v{}", index);
			// Extend the enum constructor
			if field.exists_at(current) {
				inner.extend(quote!(#field_ident,));
			}

			let (deserialize, pp) =
				field.generate_deserializer_field(index.try_into().unwrap(), current, revision);
			// Extend the deserializer
			deserializer.extend(deserialize);
			if let Some(p) = pp {
				post_process.extend(p);
			}
		}
		// Check if the variant no longer exists
		if !self.exists_at(current) {
			if let Some(conv_fn) = self.parsed.convert_fn.as_ref() {
				let convert_fn = syn::Ident::new(conv_fn, Span::call_site());
				quote! {
					#index => {
						#deserializer
						let mut object = Self::#convert_fn(revision, (#inner));
						#post_process
						return object
					},
				}
			} else {
				quote! {
					compile_error!()
				}
			}
		} else {
			// Check if this is a simple enum
			if self.fields.is_empty() {
				quote! {
					#index => {
						let mut object = Self::#ident;
						#post_process
						return Ok(object);
					},
				}
			} else {
				quote! {
					#index => {
						#deserializer
						let mut object = Self::#ident(#inner);
						#post_process
						return Ok(object);
					},
				}
			}
		}
	}
}
