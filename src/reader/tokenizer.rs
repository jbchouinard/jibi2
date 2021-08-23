use std::fmt;

use lazy_static::lazy_static;
use regex::Regex;

use crate::reader::PositionTag;
use crate::value::{FloatType, IntType};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    None,
    Char(char),
    Int(IntType),
    Float(FloatType),
    Keyword(String),
    Ident(String),
    String(String),
    Eof,
}

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenValue::*;
        match &self {
            Eof => write!(f, "#EOF"),
            None => write!(f, "#NONE"),
            Int(n) => write!(f, "Int({})", n),
            Float(x) => write!(f, "Float({})", x),
            Ident(s) => write!(f, "Ident({})", s),
            Keyword(k) => write!(f, "Keyword({})", k),
            String(s) => write!(f, "String(\"{}\")", s),
            Char(c) => write!(f, "Char('{}')", c),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub pos: PositionTag,
}

impl Token {
    pub fn new(value: TokenValue, pos: PositionTag) -> Self {
        Self { value, pos }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.value == other.value
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "token {} at {}", self.value, self.pos)
    }
}

lazy_static! {
    static ref RE_KEYWORD: Regex = Regex::new(
        r"(?x)
            ^(
            def|set!|let|begin
            |if|equal\?
            |\+|-|/|\*
            |=|!=|>|>=|<|<=
            |nil|true|false
            )
        "
    )
    .unwrap();
    static ref RE_WS: Regex = Regex::new(r"^\s+").unwrap();
    static ref RE_IDENT: Regex = Regex::new(
        r"(?x)
            ^
            ([a-zA-Z+.*/<>=!?$%_&~^-][0-9a-zA-Z+.*/<=>!?$%_&~^-]*)
            (::[a-zA-Z+.*/<>=!?$%_&~^-][0-9a-zA-Z+.*/<=>!?$%_&~^-]*)*
        "
    )
    .unwrap();
    static ref RE_STRING: Regex = Regex::new(r#"^"([^"]|\\")*""#).unwrap();
    static ref RE_FLOAT: Regex = Regex::new(r"^-?([.][0-9]+|[0-9]+[.][0-9]*)").unwrap();
    static ref RE_INT: Regex = Regex::new(r"^-?[0-9]+").unwrap();
    static ref RE_COMMENT: Regex = Regex::new(r"^;[^\n]*").unwrap();
    static ref RE_CHAR: Regex = Regex::new(r"^.").unwrap();
}

type TResult = std::result::Result<TokenValue, String>;

fn t_int(val: &str) -> TResult {
    match val.parse::<IntType>() {
        Ok(n) => Ok(TokenValue::Int(n)),
        Err(e) => Err(format!("int error: {}", e)),
    }
}

fn t_float(val: &str) -> TResult {
    match val.parse::<FloatType>() {
        Ok(n) => Ok(TokenValue::Float(n)),
        Err(e) => Err(format!("float error: {}", e)),
    }
}

fn t_ident(val: &str) -> TResult {
    Ok(TokenValue::Ident(val.to_string()))
}

fn t_string(val: &str) -> TResult {
    Ok(TokenValue::String(val[1..val.len() - 1].to_string()))
}

fn t_keyword(val: &str) -> TResult {
    Ok(TokenValue::Keyword(val.to_string()))
}

fn t_char(s: &str) -> TResult {
    Ok(TokenValue::Char(s.chars().into_iter().next().unwrap()))
}

pub struct Tokenizer {
    filename: String,
    input: String,
    pos: usize,
    lineno: usize,
    last_newline_pos: usize,
}

impl Tokenizer {
    pub fn new(filename: String, input: String) -> Self {
        Self::with_lineno(filename, input, 1)
    }
    pub fn with_lineno(filename: String, input: String, lineno: usize) -> Self {
        Self {
            filename,
            input,
            pos: 0,
            lineno,
            last_newline_pos: 0,
        }
    }
    fn ptag(&self, pos: usize) -> PositionTag {
        PositionTag {
            filename: self.filename.clone(),
            lineno: self.lineno,
            col: pos - self.last_newline_pos,
        }
    }

    fn eat_comment(&mut self) -> bool {
        match RE_COMMENT.find(&self.input[self.pos..]) {
            Some(mat) => {
                self.pos += mat.end();
                true
            }
            None => false,
        }
    }

    fn eat_whitespace(&mut self) -> bool {
        match RE_WS.find(&self.input[self.pos..]) {
            Some(mat) => {
                let spos = self.pos;
                self.pos += mat.end();
                let mut newline_count = 0;
                for (p, c) in mat.as_str().chars().enumerate() {
                    if c == '\n' {
                        newline_count += 1;
                        self.last_newline_pos = spos + p;
                    }
                }
                self.lineno += newline_count;
                true
            }
            None => false,
        }
    }

    fn try_token<T>(&mut self, re: &Regex, cons: T) -> Result<Option<Token>, LexError>
    where
        T: Fn(&str) -> TResult,
    {
        match re.find(&self.input[self.pos..]) {
            Some(mat) => {
                let spos = self.pos;
                self.pos += mat.end();
                match cons(mat.as_str()) {
                    Ok(tokval) => Ok(Some(Token::new(tokval, self.ptag(spos)))),
                    Err(reason) => Err(LexError::new(reason, self.ptag(spos))),
                }
            }
            None => Ok(None),
        }
    }
}

pub trait TokenProducer {
    fn next_token(&mut self) -> Result<Token, LexError>;
}

pub struct TokenIterator<'a, T: TokenProducer + ?Sized> {
    tokeniter: &'a mut T,
}

impl<'a, 'b, T: TokenProducer> Iterator for TokenIterator<'a, T> {
    type Item = Result<Token, LexError>;
    fn next(self: &mut TokenIterator<'a, T>) -> Option<Result<Token, LexError>> {
        match self.tokeniter.next_token() {
            Ok(tok) => match tok.value {
                TokenValue::Eof => None,
                _ => Some(Ok(tok)),
            },
            Err(e) => Some(Err(e)),
        }
    }
}

pub trait TokenToIter {
    fn to_iter(&mut self) -> TokenIterator<'_, Self>
    where
        Self: TokenProducer;
}

impl<T: TokenProducer> TokenToIter for T {
    fn to_iter(&mut self) -> TokenIterator<'_, Self> {
        TokenIterator { tokeniter: self }
    }
}

impl TokenProducer for Tokenizer {
    fn next_token(&mut self) -> Result<Token, LexError> {
        while self.eat_whitespace() || self.eat_comment() {}

        if self.pos >= self.input.len() {
            return Ok(Token::new(TokenValue::Eof, self.ptag(self.pos)));
        }
        if let Some(token) = self.try_token(&RE_FLOAT, t_float)? {
            return Ok(token);
        }
        if let Some(token) = self.try_token(&RE_INT, t_int)? {
            return Ok(token);
        }
        if let Some(token) = self.try_token(&RE_KEYWORD, t_keyword)? {
            return Ok(token);
        }
        if let Some(token) = self.try_token(&RE_IDENT, t_ident)? {
            return Ok(token);
        }
        if let Some(token) = self.try_token(&RE_STRING, t_string)? {
            return Ok(token);
        }
        if let Some(token) = self.try_token(&RE_CHAR, t_char)? {
            return Ok(token);
        }
        Err(LexError::new(
            format!(
                "unexpected character {}",
                &self.input[self.pos..self.pos + 1]
            ),
            self.ptag(self.pos),
        ))
    }
}

impl TokenProducer for std::vec::IntoIter<Token> {
    fn next_token(&mut self) -> Result<Token, LexError> {
        match self.next() {
            Some(tok) => Ok(tok),
            None => Ok(Token::new(TokenValue::Eof, PositionTag::new("", 0, 0))),
        }
    }
}

pub struct TokenValidator {
    filename: String,
    balance: Vec<TokenValue>,
    tokens: Vec<Token>,
    lineno: usize,
}

/// Balanced parens validation for multi-line input.
impl TokenValidator {
    pub fn new(filename: &str) -> Self {
        Self {
            filename: filename.to_string(),
            balance: vec![],
            tokens: vec![],
            lineno: 0,
        }
    }
    /// Returns None when more input is expected based on counting parens.
    /// Returns tokens when it looks like it may form a complete expression.
    pub fn input(&mut self, s: String) -> Result<Option<Vec<Token>>, LexError> {
        self.lineno += 1;
        let mut tokenizer = Tokenizer::with_lineno(self.filename.clone(), s, self.lineno);
        let new_toks: Vec<Token> = tokenizer
            .to_iter()
            .collect::<Result<Vec<Token>, LexError>>()?;
        for tok in new_toks {
            match tok.value {
                TokenValue::Char('(') => self.balance.push(TokenValue::Char('(')),
                TokenValue::Char(')') => match self.balance.pop() {
                    Some(TokenValue::Char('(')) => (),
                    _ => {
                        return Err(LexError::new(
                            "unexpected closing parens".to_string(),
                            tok.pos,
                        ))
                    }
                },
                _ => (),
            }
            self.tokens.push(tok);
        }
        Ok(if self.balance.is_empty() {
            self.tokens.push(Token::new(
                TokenValue::Eof,
                PositionTag::new(&self.filename, self.lineno + 1, 0),
            ));
            Some(std::mem::take(&mut self.tokens))
        } else {
            None
        })
    }
}

#[derive(Debug)]
pub struct LexError {
    pub pos: PositionTag,
    pub reason: String,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "LexError: {} at character {}", self.reason, self.pos,)
    }
}

impl LexError {
    pub fn new(reason: String, pos: PositionTag) -> Self {
        Self { pos, reason }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_tokenizer(input: &str, expected: Vec<TokenValue>) {
        let mut tokenizer = Tokenizer::new("test".to_string(), input.to_string());
        let tokens = tokenizer
            .to_iter()
            .collect::<Result<Vec<Token>, LexError>>()
            .unwrap();
        let tokvalues: Vec<TokenValue> = tokens.into_iter().map(|t| t.value).collect();
        assert_eq!(expected, tokvalues);
    }

    #[test]
    fn test_tokenizer_1() {
        test_tokenizer(
            "(* 12 -15)",
            vec![
                TokenValue::Char('('),
                TokenValue::Keyword("*".to_string()),
                TokenValue::Int(12),
                TokenValue::Int(-15),
                TokenValue::Char(')'),
            ],
        );
    }

    #[test]
    fn test_tokenizer_2() {
        test_tokenizer(
            "(concat \"foo\" \"bar\")",
            vec![
                TokenValue::Char('('),
                TokenValue::Ident("concat".to_string()),
                TokenValue::String("foo".to_string()),
                TokenValue::String("bar".to_string()),
                TokenValue::Char(')'),
            ],
        );
    }

    #[test]
    fn test_tokenizer_3() {
        test_tokenizer(
            "(quote '(1 2 3))",
            vec![
                TokenValue::Char('('),
                TokenValue::Ident("quote".to_string()),
                TokenValue::Char('\''),
                TokenValue::Char('('),
                TokenValue::Int(1),
                TokenValue::Int(2),
                TokenValue::Int(3),
                TokenValue::Char(')'),
                TokenValue::Char(')'),
            ],
        );
    }

    #[test]
    fn test_tokenizer_4() {
        test_tokenizer(
            "(quote ; this is a comment!
                '(1 2 3))",
            vec![
                TokenValue::Char('('),
                TokenValue::Ident("quote".to_string()),
                TokenValue::Char('\''),
                TokenValue::Char('('),
                TokenValue::Int(1),
                TokenValue::Int(2),
                TokenValue::Int(3),
                TokenValue::Char(')'),
                TokenValue::Char(')'),
            ],
        );
    }

    #[test]
    fn test_tokenizer_5() {
        test_tokenizer(
            "(* 12.0 -0.5)",
            vec![
                TokenValue::Char('('),
                TokenValue::Keyword("*".to_string()),
                TokenValue::Float(12.0),
                TokenValue::Float(-0.5),
                TokenValue::Char(')'),
            ],
        );
    }

    #[test]
    fn test_tokenizer_6() {
        test_tokenizer("2.025", vec![TokenValue::Float(2.025)]);
    }

    #[test]
    fn test_tokenizer_7() {
        test_tokenizer(
            "(token 'char \")\")",
            vec![
                TokenValue::Char('('),
                TokenValue::Ident("token".to_string()),
                TokenValue::Char('\''),
                TokenValue::Ident("char".to_string()),
                TokenValue::String(")".to_string()),
                TokenValue::Char(')'),
            ],
        );
    }
}
