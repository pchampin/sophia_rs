pub fn for_each_permutation_of<T, F, E>(values: &mut [T], mut f: F) -> Result<(), E>
where
    F: FnMut(&[T]) -> Result<(), E>,
{
    if !values.is_empty() {
        permutations(values, &mut f, values.len())
    } else {
        Ok(())
    }
}

// https://www.geeksforgeeks.org/heaps-algorithm-for-generating-permutations/
fn permutations<T, F, E>(values: &mut [T], f: &mut F, size: usize) -> Result<(), E>
where
    F: FnMut(&[T]) -> Result<(), E>,
{
    if size == 1 {
        f(values)?;
    } else {
        for i in 0..size {
            permutations(values, f, size - 1)?;

            if size % 2 == 1 {
                values.swap(0, size - 1);
            } else {
                values.swap(i, size - 1);
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn check_empty() {
        crate::test_setup();

        let mut a = [];
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.to_vec());
            Ok(()) as Result<(), ()>
        })
        .unwrap();
        assert!(got.is_empty());
    }

    #[test]
    fn check_1() {
        crate::test_setup();

        let mut a = [1];
        let exp = [vec![1]].into_iter().collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.to_vec());
            Ok(()) as Result<(), ()>
        })
        .unwrap();
        assert_eq!(got, exp);
    }

    #[test]
    fn check_12() {
        crate::test_setup();

        let mut a = [1, 2];
        let exp = [vec![1, 2], vec![2, 1]].into_iter().collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.to_vec());
            Ok(()) as Result<(), ()>
        })
        .unwrap();
        assert_eq!(got, exp);
    }

    #[test]
    fn check_123() {
        crate::test_setup();

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
            got.insert(a.to_vec());
            Ok(()) as Result<(), ()>
        })
        .unwrap();
        assert_eq!(got, exp);
    }

    #[test]
    fn check_1234() {
        crate::test_setup();

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
            got.insert(a.to_vec());
            Ok(()) as Result<(), ()>
        })
        .unwrap();
        assert_eq!(got, exp);
    }

    #[test]
    fn check_12345() {
        crate::test_setup();

        let mut a = [1, 2, 3, 4, 5];
        let mut got = HashSet::<Vec<i32>>::new();
        for_each_permutation_of(&mut a, |a| {
            got.insert(a.to_vec());
            Ok(()) as Result<(), ()>
        })
        .unwrap();
        assert_eq!(got.len(), 5 * 4 * 3 * 2);
    }

    #[test]
    fn check_err() {
        crate::test_setup();

        let mut a = [1, 2, 3, 4];
        let exp = [
            vec![1, 2, 3, 4],
            vec![2, 1, 3, 4],
            vec![3, 1, 2, 4],
            vec![1, 3, 2, 4],
            vec![2, 3, 1, 4],
            vec![3, 2, 1, 4],
        ]
        .into_iter()
        .collect::<HashSet<_>>();
        let mut got = HashSet::<Vec<i32>>::new();
        let res = for_each_permutation_of(&mut a, |a| {
            if a[0] == 4 {
                Err(())
            } else {
                got.insert(a.to_vec());
                Ok(())
            }
        });
        assert!(res.is_err());
        assert_eq!(got, exp);
    }
}
