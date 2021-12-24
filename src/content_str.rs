use nom::*;

#[derive(Debug, Clone)]
pub struct ContentStr<'a> {
    pub s: &'a str,
    pub name: usize,
}

pub trait IntoContentStr<'a> {
    fn into_content(self, name: usize) -> ContentStr<'a>;
}

impl<'a> IntoContentStr<'a> for &'a str {
    fn into_content(self, name: usize) -> ContentStr<'a> {
        ContentStr { s: self, name, }
    }
}

impl<'a, 'b> Compare<&'b str> for ContentStr<'a> {
    fn compare(&self, t: &'b str) -> CompareResult {
        self.s.compare(t)
    }
    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        self.s.compare_no_case(t)
    }
}

impl<'a> ExtendInto for ContentStr<'a> {
    type Item = <&'a str as ExtendInto>::Item;
    type Extender = <&'a str as ExtendInto>::Extender;

    #[inline]
    fn new_builder(&self) -> String {
        self.s.new_builder()
    }
    fn extend_into(&self, acc: &mut String) {
        self.s.extend_into(acc);
    }
}

impl<'a, 'b> FindSubstring<&'b str> for ContentStr<'a> {
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.s.find_substring(substr)
    }
}

impl<'a> FindToken<char> for ContentStr<'a> {
    fn find_token(&self, token: char) -> bool {
        self.s.find_token(token)
    }
}

impl<'a> InputIter for ContentStr<'a> {
    type Item = char;
    type Iter = <&'a str as InputIter>::Iter;
    type IterElem = <&'a str as InputIter>::IterElem;
    fn iter_indices(&self) -> Self::Iter {
        self.s.iter_indices()
    }
    fn iter_elements(&self) -> Self::IterElem {
        self.s.iter_elements()
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool {
            self.s.position(predicate)
        }
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.s.slice_index(count)
    }
}

impl<'a> InputLength for ContentStr<'a> {
    fn input_len(&self) -> usize {
        self.s.input_len()
    }
}

impl<'a> InputTake for ContentStr<'a> {
    fn take(&self, count: usize) -> Self {
        Self { name: self.name, s: self.s.take(count) }
    }
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (l, r) = self.s.take_split(count);
        (Self { name: self.name, s: l }, Self { name: self.name, s: r })
    }
}

impl<'a> UnspecializedInput for ContentStr<'a> {}

impl<'a, R> Slice<R> for ContentStr<'a> where &'a str: Slice<R> {
    fn slice(&self, range: R) -> Self {
        let s = self.s.slice(range);
        Self { name: self.name, s, }
    }
}

impl<'a> Offset for ContentStr<'a> {
    fn offset(&self, second: &Self) -> usize {
        self.s.offset(second.s)
    }
}
