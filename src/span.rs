#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end:   u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Option<Self> {
        if start <= end {
            Some(Self { start, end })
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// This function is unsafe because it does not check if `start <= end`.
    /// It is up to the caller to ensure that the invariant is upheld.
    pub unsafe fn new_unchecked(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end:   self.end.max(other.end),
        }
    }

    pub fn location(&self, input: &str) -> (Location, Location) {
        let start = self.start;
        let end = self.end;
        let mut start_location = Location::default();
        let mut line = 1;
        let mut column = 1;

        for (i, c) in input.chars().enumerate() {
            if i == start as usize {
                start_location = Location::new(line, column);
            }
            if i == end as usize {
                break;
            }
            if c == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }

        (start_location, Location::new(line, column))
    }
}

impl Default for Span {
    fn default() -> Self {
        unsafe { Self::new_unchecked(0, 0) }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line:   u32,
    pub column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
