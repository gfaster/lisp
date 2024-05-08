use std::marker::PhantomData;

pub struct ByteCodeBlock<'rt> {
    inner: PhantomData<&'rt ()>,
}
