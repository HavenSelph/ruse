pub(crate) trait Unbox<T> {
    type InnerValue;
    fn unbox(&self) -> Self::InnerValue;
}

impl<T: ToOwned> Unbox<T> for Box<T> {
    type InnerValue = T::Owned;

    fn unbox(&self) -> Self::InnerValue {
        (**self).to_owned()
    }
}
