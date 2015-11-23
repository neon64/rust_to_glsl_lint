use syntax::ast;
use rustc::middle::ty;
use rustc::middle::def_id::DefId;
use rustc_front::hir;
use rustc::metadata::csearch;
use rustc::front::map::Node::NodeItem;

/// Detects whether an element has an attribute of a certain name
///
/// For example: #[foo] or #[bar]
pub fn has_attribute(attrs: &[ast::Attribute], attribute_name: &str) -> bool {
    for attr in attrs {
        match attr.node.value.node {
            ast::MetaList(ref name, _) => {
                if *name == attribute_name { return true; }
            },
            ast::MetaNameValue(ref name, _) => {
                if *name == attribute_name { return true; }
            },
            ast::MetaWord(ref name) => {
                if *name == attribute_name { return true; }
            }
        }
    }

    false
}

/// Retrieves a property of an attribute:
///
/// #[uniform(scope="the value you want to retrieve")]
pub fn get_attribute_list_named_value(attrs: &[ast::Attribute], attribute_name: &str, attribute_property: &str) -> Option<String>  {
    for attr in attrs {
        if let ast::MetaList(ref name, ref items) = attr.node.value.node { // uniform(scope="foo")
            if *name == attribute_name {
                for item in items {
                    if let ast::MetaNameValue(ref name, ref value) = item.node { // scope="foo"
                        if *name == attribute_property {
                            if let ast::LitStr(ref string, _) = value.node {    // "foo"
                                return Some(string.to_string())
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Retrieves the first property in the list.
///
/// For example: #[shader(vertex)]
pub fn get_attribute_list_first_value(attrs: &[ast::Attribute], attribute_name: &str) -> Option<String>  {
    for attr in attrs {
        if let ast::MetaList(ref name, ref items) = attr.node.value.node { // uniform(scope="foo")
            if *name == attribute_name {
                if let Some(item) = items.first() {
                    if let ast::MetaWord(ref value) = item.node { // scope="foo"
                        return Some(value.to_string())
                    }
                }
            }
        }
    }
    None
}

/// Retrieves the value of an attribute
///
/// For example: #[foo = "qux"] or #[bar = "baz"]
pub fn get_attribute_name_value(attrs: &[ast::Attribute], attribute_name: &str) -> Option<String> {
    for attr in attrs {
        if let ast::MetaNameValue(ref name, ref value) = attr.node.value.node {
            if *name == attribute_name {
                if let ast::LitStr(ref string, _) = value.node {
                    return Some(string.to_string());
                }
            }
        }
    }

    None
}

pub fn get_struct_field_attrs(tcx: &ty::ctxt, struct_id: DefId, field_id: DefId) -> Option<Vec<ast::Attribute>> {
    if let Some(id) = tcx.map.as_local_node_id(struct_id) {
        if let Some(field_id) = tcx.map.as_local_node_id(field_id) {
            if let NodeItem(item) = tcx.map.get(id) {
                if let hir::ItemStruct(ref def, _) = item.node {
                    for field in def.fields() {
                        if field_id == field.node.id {
                            return Some(field.node.attrs.clone())
                        }
                    }
                }
            }
        }
        None
    } else {
        csearch::get_struct_field_attrs(&tcx.sess.cstore, struct_id).get(&field_id).cloned()
    }
}