/// Keeps track of the indentation level of the source
#[derive(Debug)]
pub struct Indent {
    level: i32
}

impl Indent {
    /// Returns a newline followed by the current indent
    pub fn begin_line(&self) -> String {
        let mut result = String::new();
        result.push_str("\n");
        for _ in 0..self.level {
            result.push_str("    ");
        };
        result
    }
    pub fn increment(&mut self) {
        self.level += 1
    }
    pub fn decrement(&mut self) {
        self.level -= 1
    }
    pub fn new() -> Indent {
        Indent { level: 0 }
    }
}
