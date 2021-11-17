use std::fmt;
use std::rc::Rc;

use crate::error::Error;
use crate::object::Object;
use crate::reader::PositionTag;
use crate::reader::Token;
use crate::reader::TokenProducer;
use crate::reader::TokenValue;

pub struct Parser {
    tokens: Box<dyn TokenProducer>,
    peek: Token,
}

impl Parser {
    pub fn new(tokens: Box<dyn TokenProducer>) -> Self {
        let mut this = Self {
            tokens,
            // Dummy value until we read the first real token
            peek: Token::new(TokenValue::None, PositionTag::new("", 0, 0)),
        };
        this.next().unwrap();
        this
    }

    fn error(&self, pos: PositionTag, reason: &str) -> SyntaxError {
        SyntaxError::new(pos, reason.to_string())
    }

    fn next(&mut self) -> Result<Token, SyntaxError> {
        let next = match self.tokens.next_token() {
            Ok(tok) => tok,
            Err(te) => return Err(self.error(te.pos, &te.reason)),
        };
        let cur = std::mem::replace(&mut self.peek, next);
        Ok(cur)
    }

    fn expect(&mut self, tok: TokenValue) -> Result<Token, SyntaxError> {
        let next = self.next()?;
        if next.value == tok {
            Ok(next)
        } else {
            Err(self.error(
                next.pos,
                &format!("expected token {:?}, got {:?}", tok, next.value),
            ))
        }
    }

    fn sexpr(&mut self) -> Result<Object, SyntaxError> {
        match self.peek.value {
            TokenValue::Char('(') => self.list(),
            _ => self.atom(),
        }
    }

    fn atom(&mut self) -> Result<Object, SyntaxError> {
        let next = self.next()?;
        match next.value {
            TokenValue::Int(n) => Ok(Object::Int(n)),
            TokenValue::Float(x) => Ok(Object::Float(x)),
            TokenValue::Ident(s) => Ok(Object::Symbol(Rc::new(s))),
            TokenValue::String(s) => Ok(Object::String(Rc::new(s))),
            _ => Err(self.error(next.pos, &format!("unexpected token {:?}", next.value))),
        }
    }

    fn list(&mut self) -> Result<Object, SyntaxError> {
        self.expect(TokenValue::Char('('))?;
        let mut list = vec![];
        while self.peek.value != TokenValue::Char(')') {
            list.push(self.sexpr()?);
        }
        self.expect(TokenValue::Char(')'))?;
        Ok(Object::make_list(list))
    }

    pub fn parse(&mut self) -> Result<Option<(Object, PositionTag)>, SyntaxError> {
        if self.peek.value == TokenValue::Eof {
            return Ok(None);
        }
        let spos = self.peek.pos.clone();
        match self.sexpr() {
            Ok(val) => Ok(Some((val, spos))),
            Err(e) => Err(e),
        }
    }
}

impl Iterator for Parser {
    type Item = Result<(Object, PositionTag), Error>;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.parse() {
            Ok(None) => None,
            Ok(Some(obj)) => Some(Ok(obj)),
            Err(e) => Some(Err(e.into())),
        }
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    pub pos: PositionTag,
    pub reason: String,
}

impl SyntaxError {
    pub fn new(pos: PositionTag, reason: String) -> Self {
        Self { pos, reason }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "SyntaxError: {} at {}", self.reason, self.pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::tokenizer::Tokenizer;

    fn rcs(s: &str) -> Rc<String> {
        Rc::new(s.to_string())
    }

    fn test_parser(input: &str, expected: Object) {
        let mut parser = Parser::new(Box::new(Tokenizer::new(
            "test".to_string(),
            input.to_string(),
        )));
        let val = parser.sexpr().unwrap();
        assert_eq!(expected, val);
    }

    #[test]
    fn test_parser_1() {
        let lst = vec![Object::Symbol(rcs("+")), Object::Int(12), Object::Int(-15)];
        let expected = Object::make_list(lst);
        test_parser("(+ 12 -15)", expected);
    }

    #[test]
    fn test_parser_2() {
        let inner_lst = vec![Object::Symbol(rcs("+")), Object::Int(12), Object::Int(-33)];
        let lst = vec![
            Object::Symbol(rcs("*")),
            Object::make_list(inner_lst),
            Object::Int(42),
        ];
        let expected = Object::make_list(lst);
        test_parser("(* (+ 12 -33) 42)", expected)
    }

    #[test]
    fn test_parser_3() {
        let lst = vec![
            Object::Symbol(rcs("concat")),
            Object::String(rcs("foo")),
            Object::String(rcs("bar")),
        ];
        let expected = Object::make_list(lst);
        test_parser("(concat \"foo\" \"bar\")", expected)
    }

    #[test]
    fn test_parser_4() {
        let lst = vec![
            Object::Symbol(rcs("+")),
            Object::Float(12.0),
            Object::Float(-0.5),
        ];
        let expected = Object::make_list(lst);
        test_parser("(+ 12.0 -0.5)", expected);
    }

    #[test]
    fn test_parser_5() {
        let expected = Object::Float(2.025);
        test_parser("2.025", expected);
    }
}
