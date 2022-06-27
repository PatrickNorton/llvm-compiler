use std::borrow::Cow;

use super::user::UserTypeInner;

pub fn get_defined<T: UserTypeInner + ?Sized>(parent: &'_ T) -> impl Iterator<Item = Cow<'_, str>> {
    let info = parent.get_info();
    let attrs = info.attributes.get().unwrap().keys();
    attrs
        .map(|x| x.as_str().into())
        .chain(info.supers.iter().filter_map(|x| x.get_defined()).flatten())
}

pub fn static_defined<T: UserTypeInner + ?Sized>(parent: &'_ T) -> impl Iterator<Item = &'_ str> {
    let info = parent.get_info();
    let static_attrs = info.static_attributes.get().unwrap().keys();
    static_attrs.map(|x| x.as_str()).chain(
        info.supers
            .iter()
            .filter_map(|x| x.static_defined())
            .flatten(),
    )
}
