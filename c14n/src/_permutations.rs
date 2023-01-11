pub fn for_each_permutation_of<T, F>(values: &mut [T], mut f: F)
where
    F: FnMut(&[T]),
{
    if !values.is_empty() {
        permutations(values, &mut f, values.len());
    }
}

// https://www.geeksforgeeks.org/heaps-algorithm-for-generating-permutations/
fn permutations<T, F>(values: &mut [T], f: &mut F, size: usize)
where
    F: FnMut(&[T]),
{
    if size == 1 {
        f(values);
    } else {
        for i in 0..size {
            permutations(values, f, size - 1);

            if size % 2 == 1 {
                values.swap(0, size - 1);
            } else {
                values.swap(i, size - 1);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn check_empty() {
        let mut a = [];
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.into_iter().copied().collect());
        });
        assert!(got.is_empty());
    }

    #[test]
    fn check_1() {
        let mut a = [1];
        let exp = [vec![1]].into_iter().collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.into_iter().copied().collect());
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn check_12() {
        let mut a = [1, 2];
        let exp = [vec![1, 2], vec![2, 1]].into_iter().collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.into_iter().copied().collect());
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn check_123() {
        let mut a = [1, 2, 3];
        let exp = [
            vec![1, 2, 3],
            vec![2, 1, 3],
            vec![3, 1, 2],
            vec![1, 3, 2],
            vec![2, 3, 1],
            vec![3, 2, 1],
        ]
        .into_iter()
        .collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.into_iter().copied().collect());
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn check_1234() {
        let mut a = [1, 2, 3, 4];
        let exp = [
            vec![1, 2, 3, 4],
            vec![2, 1, 3, 4],
            vec![3, 1, 2, 4],
            vec![1, 3, 2, 4],
            vec![2, 3, 1, 4],
            vec![3, 2, 1, 4],
            vec![4, 2, 3, 1],
            vec![2, 4, 3, 1],
            vec![3, 4, 2, 1],
            vec![4, 3, 2, 1],
            vec![2, 3, 4, 1],
            vec![3, 2, 4, 1],
            vec![4, 1, 3, 2],
            vec![1, 4, 3, 2],
            vec![3, 4, 1, 2],
            vec![4, 3, 1, 2],
            vec![1, 3, 4, 2],
            vec![3, 1, 4, 2],
            vec![4, 1, 2, 3],
            vec![1, 4, 2, 3],
            vec![2, 4, 1, 3],
            vec![4, 2, 1, 3],
            vec![1, 2, 4, 3],
            vec![2, 1, 4, 3],
        ]
        .into_iter()
        .collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.into_iter().copied().collect());
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn check_12345() {
        let mut a = [1, 2, 3, 4, 5];
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.into_iter().copied().collect());
        });
        assert_eq!(got.len(), 5 * 4 * 3 * 2);
    }
}
