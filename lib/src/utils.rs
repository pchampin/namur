pub fn mappings<'a, T, U, F, E>(ts: &'a [T], us: &'a [U], mut callback: F) -> Result<(), E>
where
    F: FnMut(&[(&'a T, &'a U)]) -> Result<(), E>,
{
    let mut buffer = Vec::with_capacity(ts.len());
    mappings_rec(ts, us, &mut callback, &mut buffer)
}

fn mappings_rec<'a, T, U, F, E>(
    ts: &'a [T],
    us: &'a [U],
    callback: &mut F,
    buffer: &mut Vec<(&'a T, &'a U)>,
) -> Result<(), E>
where
    F: FnMut(&[(&'a T, &'a U)]) -> Result<(), E>,
{
    if ts.is_empty() {
        callback(&buffer[..])
    } else {
        for u in us {
            buffer.push((&ts[0], u));
            mappings_rec(&ts[1..], us, callback, buffer)?;
            buffer.pop();
        }
        Ok(())
    }
}

pub fn injections<'a, T, U, F, E>(ts: &'a [T], us: &'a [U], mut callback: F) -> Result<(), E>
where
    F: FnMut(&[(&'a T, &'a U)]) -> Result<(), E>,
{
    if ts.len() > us.len() {
        return Ok(());
    }
    let mut buffer = Vec::with_capacity(ts.len());
    let mut mask = vec![false; us.len()];
    injections_rec(ts, us, &mut callback, &mut buffer, &mut mask)
}

fn injections_rec<'a, T, U, F, E>(
    ts: &'a [T],
    us: &'a [U],
    callback: &mut F,
    buffer: &mut Vec<(&'a T, &'a U)>,
    mask: &mut [bool],
) -> Result<(), E>
where
    F: FnMut(&[(&'a T, &'a U)]) -> Result<(), E>,
{
    if ts.is_empty() {
        callback(&buffer[..])
    } else {
        for (i, u) in us.iter().enumerate() {
            if mask[i] {
                continue;
            }
            mask[i] = true;
            buffer.push((&ts[0], u));
            injections_rec(&ts[1..], us, callback, buffer, mask)?;
            buffer.pop();
            mask[i] = false;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    type MyMapping = Vec<(&'static usize, &'static usize)>;

    #[test_case(&[], &[10, 20] => vec![MyMapping::new()]; "0 vs 2")]
    #[test_case(&[1], &[10] => vec![vec![(&1, &10)]]; "1 vs 1")]
    #[test_case(&[1], &[10, 20] => vec![vec![(&1, &10)], vec![(&1, &20)]]; "1 vs 2")]
    #[test_case(&[1, 2], &[10] => vec![vec![(&1, &10), (&2, &10)]]; "2 vs 1")]
    #[test_case(&[1, 2], &[10, 20] => vec![
        vec![(&1, &10), (&2, &10)],
        vec![(&1, &10), (&2, &20)],
        vec![(&1, &20), (&2, &10)],
        vec![(&1, &20), (&2, &20)],
    ]; "2 vs 2")]
    #[test_case(&[1, 2], &[10, 20, 30] => vec![
        vec![(&1, &10), (&2, &10)],
        vec![(&1, &10), (&2, &20)],
        vec![(&1, &10), (&2, &30)],
        vec![(&1, &20), (&2, &10)],
        vec![(&1, &20), (&2, &20)],
        vec![(&1, &20), (&2, &30)],
        vec![(&1, &30), (&2, &10)],
        vec![(&1, &30), (&2, &20)],
        vec![(&1, &30), (&2, &30)],
    ]; "2 vs 3")]
    fn test_mappings(ts: &'static [usize], us: &'static [usize]) -> Vec<MyMapping> {
        let mut ret = vec![];
        mappings(ts, us, |m| -> Result<(), String> {
            ret.push(m.iter().cloned().collect());
            Ok(())
        })
        .unwrap();
        ret
    }

    #[test_case(&[], &[10, 20] => vec![MyMapping::new()]; "0 vs 2")]
    #[test_case(&[1], &[10] => vec![vec![(&1, &10)]]; "1 vs 1")]
    #[test_case(&[1], &[10, 20] => vec![vec![(&1, &10)], vec![(&1, &20)]]; "1 vs 2")]
    #[test_case(&[1, 2], &[10] => Vec::<MyMapping>::new(); "2 vs 1")]
    #[test_case(&[1, 2], &[10, 20] => vec![
        vec![(&1, &10), (&2, &20)],
        vec![(&1, &20), (&2, &10)],
    ]; "2 vs 2")]
    #[test_case(&[1, 2], &[10, 20, 30] => vec![
        vec![(&1, &10), (&2, &20)],
        vec![(&1, &10), (&2, &30)],
        vec![(&1, &20), (&2, &10)],
        vec![(&1, &20), (&2, &30)],
        vec![(&1, &30), (&2, &10)],
        vec![(&1, &30), (&2, &20)],
    ]; "2 vs 3")]
    fn test_injections(ts: &'static [usize], us: &'static [usize]) -> Vec<MyMapping> {
        let mut ret = vec![];
        injections(ts, us, |m| -> Result<(), String> {
            ret.push(m.iter().cloned().collect());
            Ok(())
        })
        .unwrap();
        ret
    }
}
