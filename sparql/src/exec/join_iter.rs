pub enum JoinIter<E, I> {
    PassThrough(I),
    Err(E),
    Finish,
}

impl<T, E, I> Iterator for JoinIter<E, I>
where
    I: Iterator<Item = Result<T, E>>,
{
    type Item = Result<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut tmp = Self::Finish;
        std::mem::swap(self, &mut tmp);
        match tmp {
            Self::PassThrough(mut iter) => {
                let ret = iter.next();
                if ret.is_some() {
                    *self = Self::PassThrough(iter);
                }
                ret
            }
            Self::Err(err) => Some(Err(err)),
            Self::Finish => None,
        }
    }
}
