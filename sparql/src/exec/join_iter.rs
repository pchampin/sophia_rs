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
        let mut tmp = JoinIter::Finish;
        std::mem::swap(self, &mut tmp);
        match tmp {
            JoinIter::PassThrough(mut iter) => {
                let ret = iter.next();
                if ret.is_some() {
                    *self = JoinIter::PassThrough(iter);
                }
                ret
            }
            JoinIter::Err(err) => Some(Err(err)),
            JoinIter::Finish => None,
        }
    }
}
