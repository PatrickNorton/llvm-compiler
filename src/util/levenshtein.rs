use std::cmp::{max, min};

/// Find the closest string to the given name by Levenshtein distance, returning
/// [`None`] when no options are close enough.
///
/// "Close enough" in this case is defined as having a Levenshtein distance
/// less than 1/3 the length of `name`.
///
/// # Examples
/// ```
/// let names = vec!["foo", "bar", "baz"];
/// assert_eq!(closest_name("for", names.iter().copied()), Some("foo"));
/// assert_eq!(closest_name("qqq", names.iter().copied()), None);
/// ```
pub fn closest_name<S>(name: &str, names: impl Iterator<Item = S>) -> Option<S>
where
    S: AsRef<str>,
{
    let (min, dist) = names
        .filter(|x| !x.as_ref().is_empty())
        .map(|x| {
            let distance = distance(name, x.as_ref());
            (x, distance)
        })
        .min_by_key(|x| x.1)?;
    let max_distance = max(name.len(), 3) / 3;
    (dist <= max_distance).then_some(min)
}

/// Find the Levenshtein distance between two strings.
///
/// The Levenshtein distance is the number of single-character edits required to
/// transform one string into the other. An edit is either an insertion,
/// deletion, or substitution.
///
/// # Examples
/// ```
/// assert_eq!(distance("abcde", "bcdef"), 2);
/// assert_eq!(distance("foo", "bar"), 3);
/// assert_eq!(distance("identical", "identical"), 0);
/// ```
fn distance(a: &str, b: &str) -> usize {
    if a.is_empty() {
        return b.len();
    } else if b.is_empty() {
        return a.len();
    }

    let mut d_col = (0..=b.len()).collect::<Vec<_>>();
    let mut t_last = 0;

    for (i, sc) in a.chars().enumerate() {
        let mut current = i;
        d_col[0] = current + 1;

        for (j, tc) in b.chars().enumerate() {
            let next = d_col[j + 1];
            if sc == tc {
                d_col[j + 1] = current;
            } else {
                d_col[j + 1] = min(current, next);
                d_col[j + 1] = min(d_col[j + 1], d_col[j]) + 1;
            }
            current = next;
            t_last = j;
        }
    }
    d_col[t_last + 1]
}

#[cfg(test)]
mod tests {
    use super::distance;

    #[test]
    fn identical() {
        assert_eq!(distance("abc", "abc"), 0);
        assert_eq!(distance("", ""), 0);
    }

    #[test]
    fn empty() {
        for text in &["a", "ab", "abc", "abcd", "abcde"] {
            assert_eq!(distance(text, ""), text.len());
        }
    }

    #[test]
    fn general() {
        assert_eq!(distance("sitting", "kitten"), 3);
        assert_eq!(distance("gumbo", "gambol"), 2);
        assert_eq!(distance("saturday", "sunday"), 3);
        assert_eq!(distance("a", "b"), 1);
        assert_eq!(distance("ab", "ac"), 1);
        assert_eq!(distance("ac", "bc"), 1);
        assert_eq!(distance("abc", "axc"), 1);
        assert_eq!(distance("xabxcdxxefxgx", "1ab2cd34ef5g6"), 6);
        assert_eq!(distance("xabxcdxxefxgx", "abcdefg"), 6);
        assert_eq!(distance("example", "samples"), 3);
        assert_eq!(distance("sturgeon", "urgently"), 6);
        assert_eq!(distance("levenshtein", "frankenstein"), 6);
        assert_eq!(distance("distance", "difference"), 5);
        assert_eq!(distance("kitten", "sitting"), 3);
        assert_eq!(distance("Tier", "Tor"), 2);
    }
}
