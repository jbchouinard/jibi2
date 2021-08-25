use std::convert::TryInto;
use std::fmt;
use std::rc::Rc;

use crate::error::{Error, Result};
use crate::instruction::*;
use crate::object::{Function, FunctionRef, Object};
use crate::reader::{PositionTag, Token, TokenProducer, TokenValue, Tokenizer};
use crate::vm::{CallFrame, VM};

const TOK_LPAR: TokenValue = TokenValue::Char('(');
const TOK_RPAR: TokenValue = TokenValue::Char(')');

pub fn compile_tokens(name: &str, producer: Box<dyn TokenProducer>) -> Result<FunctionRef> {
    let compiler = Compiler::new(Rc::new(name.to_string()));
    let mut parser = Parser::new(compiler, producer);
    parser.advance()?;
    parser.parse()?;
    Ok(parser.end())
}

pub fn compile_source(filename: &str, source: &str) -> Result<FunctionRef> {
    let tokens = Tokenizer::new(filename.to_string(), source.to_string());
    compile_tokens(&format!("<script \"{}\">", filename), Box::new(tokens))
}

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: i32,
}

impl Local {
    pub fn new(name: String, depth: i32) -> Self {
        Self { name, depth }
    }
}

pub enum Variable {
    Global,
    Local(usize),
}

pub struct Compiler {
    function: FunctionRef,
    scope_depth: i32,
    locals: Vec<Local>,
    lists: Vec<usize>,
    pos: PositionTag,
    discard: bool,
}

impl Compiler {
    pub fn new(name: Rc<String>) -> Self {
        let function = Function::new(name);
        Self {
            function: function.to_ref(),
            scope_depth: 0,
            locals: vec![Local::new("".to_string(), 0)],
            lists: vec![],
            pos: PositionTag::new("", 0, 0),
            discard: false,
        }
    }
    fn offset(&self) -> usize {
        self.function.borrow().code.count()
    }
    fn emit_instruction<I: Into<AnyInstruction>>(&mut self, ins: I) {
        if self.discard {
            return;
        }
        self.function
            .borrow_mut()
            .code
            .write_instruction(ins, self.pos.lineno);
    }
    fn emit_constant(&mut self, val: Object) {
        if self.discard {
            return;
        }
        self.function
            .borrow_mut()
            .code
            .write_constant(val, self.pos.lineno);
    }
    fn emit_jump(&mut self, op: Op) -> usize {
        if self.discard {
            return 0;
        }
        self.function
            .borrow_mut()
            .code
            .write_instruction(Instruction::new(op, [0xff, 0xff]), self.pos.lineno)
    }
    fn patch_jump(&mut self, jmp: usize) {
        if self.discard {
            return;
        }
        let loc = self.offset() - jmp - 3;
        if loc > u16::MAX as usize {
            panic!("jump offset too large");
        }
        let [b1, b2] = make_long_operands(loc.try_into().unwrap());
        self.function.borrow_mut().code.write_at(jmp + 1, b1);
        self.function.borrow_mut().code.write_at(jmp + 2, b2);
    }
    fn define_variable(&mut self, name: String) -> Result<()> {
        if self.discard {
            return Ok(());
        }
        // Global variable: store in globals
        if self.scope_depth == 0 {
            self.emit_constant(Object::Symbol(Rc::new(name)));
            self.emit_instruction(instruction_def_global());
            Ok(())
        }
        // Local variable: register local, leave value on stack
        // since locals are saved on the stack, definitions can only happen at the
        // top of a new local scope
        else {
            if self.locals.len() >= u16::MAX.into() {
                panic!("too many local variables")
            }
            for local in self.locals.iter().rev() {
                if local.depth != -1 && local.depth < self.scope_depth {
                    break;
                }
                if local.name == name {
                    return Err(SyntaxError::new(
                        self.pos.clone(),
                        format!("cannot re-define local variable {}", name),
                    )
                    .into());
                }
            }
            self.locals.push(Local::new(name, self.scope_depth));
            Ok(())
        }
    }
    fn resolve_variable(&mut self, name: &str) -> Variable {
        if self.discard {
            return Variable::Global;
        }
        if !self.locals.is_empty() {
            for i in (0..self.locals.len()).rev() {
                if self.locals[i].name == name {
                    return Variable::Local(i);
                }
            }
        }
        Variable::Global
    }
    fn get_variable(&mut self, name: String) -> Result<()> {
        if self.discard {
            return Ok(());
        }
        let var = self.resolve_variable(&name);
        match var {
            Variable::Global => {
                self.emit_constant(Object::Symbol(Rc::new(name)));
                self.emit_instruction(instruction_get_global());
            }
            Variable::Local(n) => {
                self.emit_instruction(instruction_get_local(n));
            }
        }
        Ok(())
    }
    fn set_variable(&mut self, name: String) -> Result<()> {
        if self.discard {
            return Ok(());
        }
        let var = self.resolve_variable(&name);
        match var {
            Variable::Global => {
                self.emit_constant(Object::Symbol(Rc::new(name)));
                self.emit_instruction(instruction_set_global());
            }
            Variable::Local(n) => {
                self.emit_instruction(instruction_set_local(n));
            }
        }
        Ok(())
    }
    fn begin_scope(&mut self) {
        if self.discard {
            return;
        }
        self.scope_depth += 1;
    }
    fn end_scope(&mut self) {
        if self.discard {
            return;
        }
        self.scope_depth -= 1;
        let mut popped: u8 = 0;
        while !self.locals.is_empty() && self.locals[self.locals.len() - 1].depth > self.scope_depth
        {
            self.locals.pop().unwrap();
            popped += 1;
            if popped == u8::MAX {
                self.emit_instruction(instruction_pop_n(popped));
                popped = 0;
            }
        }
        if popped > 0 {
            self.emit_instruction(instruction_pop_n(popped))
        }
    }
    fn begin_list(&mut self) {
        if self.discard {
            return;
        }
        self.lists.push(self.offset());
    }
    fn end_list(&mut self) {
        if self.discard {
            return;
        }
        let start = self.lists.pop().unwrap();
        // Pre-evaluate static expressions; if there is an expression containing only
        // constants, for example (+ 15 15), we can evaluate it at compile time
        // and just write the result in the compiled chunk
        if self.is_static_expr(start) {
            let res = self.pre_evaluate(start);
            self.emit_constant(res);
        }
    }
    fn start_discard(&mut self) {
        self.discard = true;
    }
    fn end_discard(&mut self) {
        self.discard = false;
    }
    fn pre_evaluate(&mut self, start: usize) -> Object {
        self.emit_instruction(instruction_return());

        #[cfg(debug_trace_compile)]
        self.function
            .borrow()
            .code
            .disassemble("static expression", start);

        // Temporarily change arity to prevent popping on return
        let real_arity = std::mem::replace(&mut self.function.borrow_mut().arity, 0);

        let mut vm = VM::new();
        vm.stack.push(Object::Function(Rc::clone(&self.function)));
        vm.frames
            .push(CallFrame::new(Rc::clone(&self.function), start, 0));
        vm.run().unwrap();
        let res = vm.register0.take().unwrap();

        self.function.borrow_mut().code.erase(start);
        self.function.borrow_mut().arity = real_arity;

        #[cfg(debug_trace_compile)]
        println!("evaluated to: {}", res);

        res
    }
    fn is_static_expr(&self, start: usize) -> bool {
        if self.discard {
            return false;
        }
        let mut pos = start;
        while pos < self.offset() {
            let (ins, newpos) = AnyInstruction::read(&self.function.borrow().code.code, pos);
            if !ins.is_static() {
                return false;
            }
            pos = newpos;
        }
        true
    }
    pub fn end(&mut self) -> FunctionRef {
        self.emit_instruction(instruction_return());
        #[cfg(debug_trace_compile)]
        self.function
            .borrow()
            .code
            .disassemble(&format!("{}", self.function.borrow()), 0);
        std::mem::replace(
            &mut self.function,
            Function::new(Rc::new("".to_string())).to_ref(),
        )
    }
}

pub struct Parser {
    producer: Box<dyn TokenProducer>,
    peek: Token,
    stashed_next: Option<Token>,
    compilers: Vec<Compiler>,
}

macro_rules! compiler {
    ($self:ident) => {
        $self.compilers.last_mut().unwrap()
    };
}

impl Parser {
    pub fn new(compiler: Compiler, producer: Box<dyn TokenProducer>) -> Self {
        Self {
            producer,
            peek: Token::new(TokenValue::None, PositionTag::new("", 1, 0)),
            stashed_next: None,
            compilers: vec![compiler],
        }
    }
    fn advance(&mut self) -> Result<Token> {
        let stashed_next = std::mem::take(&mut self.stashed_next);
        let next = match stashed_next {
            Some(next) => std::mem::replace(&mut self.peek, next),
            None => std::mem::replace(&mut self.peek, self.producer.next_token()?),
        };
        if next.value == TokenValue::Eof {
            return Err(self.error_at(&next, "reached EOF".to_string()));
        }
        compiler!(self).pos = self.peek.pos.clone();
        Ok(next)
    }
    fn rewind(&mut self, tok: Token) {
        if self.stashed_next.is_some() {
            panic!("cannot rewind more than once");
        }
        self.stashed_next = Some(std::mem::replace(&mut self.peek, tok));
    }
    fn expect(&mut self, v: TokenValue) -> Result<Token> {
        if self.peek.value == v {
            self.advance()
        } else {
            Err(self.error(format!("expected {}, got {}", v, self.peek.value)))
        }
    }
    pub fn parse(&mut self) -> Result<()> {
        // Empty program, return nil
        if self.peek.value == TokenValue::Eof {
            compiler!(self).emit_constant(Object::Nil);
            return Ok(());
        }
        while self.peek.value != TokenValue::Eof {
            self.top_level_expression()?;
            if self.peek.value != TokenValue::Eof {
                compiler!(self).emit_instruction(instruction_pop());
            }
        }
        Ok(())
    }
    pub fn top_level_expression(&mut self) -> Result<()> {
        if !self.definition()? {
            self.expression()?;
        }
        Ok(())
    }
    pub fn definition(&mut self) -> Result<bool> {
        let next = self.advance()?;
        if next.value == TOK_LPAR && self.peek.value == TokenValue::Keyword("def".to_string()) {
            self.advance()?;
            self.sform_def()?;
            self.expect(TOK_RPAR)?;
            return Ok(true);
        }
        self.rewind(next);
        Ok(false)
    }
    pub fn expression(&mut self) -> Result<()> {
        match self.peek.value {
            TokenValue::Char('(') => self.list(),
            _ => self.atom(),
        }
    }
    fn atom(&mut self) -> Result<()> {
        let current = self.advance()?;
        match current.value {
            TokenValue::Int(n) => compiler!(self).emit_constant(Object::Int(n)),
            TokenValue::Float(x) => compiler!(self).emit_constant(Object::Float(x)),
            TokenValue::String(s) => compiler!(self).emit_constant(Object::String(Rc::new(s))),
            TokenValue::Ident(s) => compiler!(self).get_variable(s)?,
            TokenValue::Keyword(k) => self.atom_const(k)?,
            _ => return Err(self.error_at(&current, format!("unexpected {}", current.value))),
        };
        Ok(())
    }
    pub fn atom_const(&mut self, sym: String) -> Result<()> {
        match &sym[..] {
            "nil" => compiler!(self).emit_constant(Object::Nil),
            "true" => compiler!(self).emit_constant(Object::Bool(true)),
            "false" => compiler!(self).emit_constant(Object::Bool(false)),
            _ => return Err(self.error(format!("ill-formed special form {}", sym))),
        }
        Ok(())
    }
    fn list(&mut self) -> Result<()> {
        compiler!(self).begin_list();
        self.expect(TOK_LPAR)?;
        match &self.peek.value {
            TokenValue::Keyword(_) => self.special_form()?,
            TokenValue::Char('(') => compiler!(self).emit_constant(Object::Nil),
            _ => self.list_apply()?,
        };
        self.expect(TOK_RPAR)?;
        compiler!(self).end_list();
        Ok(())
    }
    fn list_apply(&mut self) -> Result<()> {
        self.expression()?;
        let mut n: u8 = 0;
        while self.peek.value != TOK_RPAR {
            self.expression()?;
            n += 1;
        }
        compiler!(self).emit_instruction(instruction_apply(n));
        Ok(())
    }
    fn ident(&mut self) -> Result<String> {
        let current = self.advance()?;
        let ident = match current.value {
            TokenValue::Ident(s) => s,
            _ => {
                return Err(
                    self.error_at(&current, format!("expected ident, got {}", current.value))
                )
            }
        };
        Ok(ident)
    }
    fn special_form(&mut self) -> Result<()> {
        let current = self.advance()?;
        let keyword = match current.value {
            TokenValue::Keyword(k) => k,
            _ => panic!("expected keyword"),
        };
        match &keyword[..] {
            "def" => return Err(self.error("def not at top level or top of scope".to_string())),
            "set!" => self.sform_set()?,
            "fn" => self.sform_fn()?,
            "let" => self.sform_let()?,
            "begin" => self.sform_begin()?,
            "if" => self.sform_if()?,
            "cond" => self.sform_cond()?,
            "equal?" => self.sform_equal()?,
            "print" => self.sform_print()?,
            "repr" => self.sform_repr()?,
            "+" => self.sform_arith(keyword)?,
            "-" => self.sform_arith(keyword)?,
            "*" => self.sform_arith(keyword)?,
            "/" => self.sform_arith(keyword)?,
            "=" => self.sform_numcmp(keyword)?,
            "!=" => self.sform_numcmp(keyword)?,
            "<" => self.sform_numcmp(keyword)?,
            "<=" => self.sform_numcmp(keyword)?,
            ">" => self.sform_numcmp(keyword)?,
            ">=" => self.sform_numcmp(keyword)?,
            _ => return Err(self.error(format!("invalid special form keyword {}", keyword))),
        }
        Ok(())
    }
    fn sform_def(&mut self) -> Result<()> {
        let ident = self.ident()?;
        self.expression()?;
        compiler!(self).define_variable(ident)?;
        compiler!(self).emit_constant(Object::Nil);
        Ok(())
    }
    fn sform_set(&mut self) -> Result<()> {
        let ident = self.ident()?;
        self.expression()?;
        compiler!(self).set_variable(ident)?;
        compiler!(self).emit_constant(Object::Nil);
        Ok(())
    }
    fn sform_fn(&mut self) -> Result<()> {
        self.compilers
            .push(Compiler::new(Rc::new("unnamed".to_string())));
        compiler!(self).begin_scope();
        self.expect(TOK_LPAR)?;
        while self.peek.value != TOK_RPAR {
            let ident = self.ident()?;
            compiler!(self).function.borrow_mut().arity += 1;
            compiler!(self).define_variable(ident)?;
        }
        self.expect(TOK_RPAR)?;
        self.block()?;
        let func = self.compilers.pop().unwrap().end();
        compiler!(self).emit_constant(Object::Function(func));
        Ok(())
    }
    fn sform_let(&mut self) -> Result<()> {
        compiler!(self).begin_scope();
        self.expect(TOK_LPAR)?;
        while self.peek.value != TOK_RPAR {
            self.expect(TOK_LPAR)?;
            let ident = self.ident()?;
            self.expression()?;
            compiler!(self).define_variable(ident)?;
            self.expect(TOK_RPAR)?;
        }
        self.expect(TOK_RPAR)?;
        while self.peek.value != TOK_RPAR {
            self.expression()?;
            compiler!(self).emit_instruction(instruction_pop_r0());
        }
        compiler!(self).end_scope();
        compiler!(self).emit_instruction(instruction_push_r0());
        Ok(())
    }
    fn block(&mut self) -> Result<()> {
        // (begin): don't even bother, just return nil
        if self.peek.value == TOK_RPAR {
            compiler!(self).emit_constant(Object::Nil);
            return Ok(());
        }
        // Save value of last expression in register, push it back at the end
        compiler!(self).begin_scope();
        while self.definition()? {
            compiler!(self).emit_instruction(instruction_pop_r0());
        }
        while self.peek.value != TOK_RPAR {
            self.expression()?;
            compiler!(self).emit_instruction(instruction_pop_r0());
        }
        compiler!(self).end_scope();
        compiler!(self).emit_instruction(instruction_push_r0());
        Ok(())
    }
    fn sform_begin(&mut self) -> Result<()> {
        self.block()?;
        Ok(())
    }
    fn sform_if(&mut self) -> Result<()> {
        let start = compiler!(self).offset();
        self.expression()?;
        // If the predicate is a static expression, don't bother with jumps, just
        // compile the correct branch
        if compiler!(self).is_static_expr(start) {
            let res = compiler!(self).pre_evaluate(start).as_bool()?;
            if res {
                self.expression()?;
                compiler!(self).start_discard();
                self.expression()?;
                compiler!(self).end_discard();
            } else {
                compiler!(self).start_discard();
                self.expression()?;
                compiler!(self).end_discard();
                self.expression()?;
            }
        } else {
            let to_else = compiler!(self).emit_jump(Op::JumpFalse);
            compiler!(self).emit_instruction(instruction_pop());
            self.expression()?;
            let to_end = compiler!(self).emit_jump(Op::Jump);
            compiler!(self).patch_jump(to_else);
            compiler!(self).emit_instruction(instruction_pop());
            self.expression()?;
            compiler!(self).patch_jump(to_end);
        }
        Ok(())
    }
    fn sform_cond(&mut self) -> Result<()> {
        let mut to_end: Vec<usize> = vec![];

        let mut discard_rest = false;
        while self.peek.value != TOK_RPAR {
            self.expect(TOK_LPAR)?;
            let start = compiler!(self).offset();
            self.expression()?;
            if discard_rest {
                self.expression()?;
            } else if compiler!(self).is_static_expr(start) {
                if compiler!(self).pre_evaluate(start).as_bool()? {
                    self.expression()?;
                    compiler!(self).start_discard();
                    discard_rest = true;
                } else {
                    compiler!(self).start_discard();
                    self.expression()?;
                    compiler!(self).end_discard();
                }
            } else {
                let skip = compiler!(self).emit_jump(Op::JumpFalse);
                compiler!(self).emit_instruction(instruction_pop());
                self.expression()?;
                to_end.push(compiler!(self).emit_jump(Op::Jump));
                compiler!(self).patch_jump(skip);
                compiler!(self).emit_instruction(instruction_pop());
            }
            self.expect(TOK_RPAR)?;
        }
        compiler!(self).end_discard();
        // In case no condition is fulfilled
        if !discard_rest {
            compiler!(self).emit_constant(Object::Nil);
        }
        for jmp in to_end {
            compiler!(self).patch_jump(jmp);
        }
        Ok(())
    }
    fn sform_equal(&mut self) -> Result<()> {
        self.expression()?;
        self.expression()?;
        compiler!(self).emit_instruction(instruction_equal());
        Ok(())
    }
    fn sform_print(&mut self) -> Result<()> {
        self.expression()?;
        compiler!(self).emit_instruction(instruction_print());
        Ok(())
    }
    fn sform_repr(&mut self) -> Result<()> {
        self.expression()?;
        compiler!(self).emit_instruction(instruction_repr());
        Ok(())
    }
    fn sform_arith(&mut self, op: String) -> Result<()> {
        let mut nargs: usize = 0;
        while self.peek.value != TOK_RPAR {
            self.expression()?;
            nargs += 1;
        }
        compiler!(self).emit_instruction(match &op[..] {
            "+" => instruction_add(nargs),
            "-" => instruction_sub(nargs),
            "*" => instruction_mul(nargs),
            "/" => instruction_div(nargs),
            _ => panic!("invalid arith keyword {}", op),
        });
        Ok(())
    }
    fn sform_numcmp(&mut self, op: String) -> Result<()> {
        self.expression()?;
        self.expression()?;
        compiler!(self).emit_instruction(match &op[..] {
            "=" => instruction_num_eq(),
            "!=" => instruction_num_neq(),
            "<" => instruction_num_lt(),
            "<=" => instruction_num_lte(),
            ">" => instruction_num_gt(),
            ">=" => instruction_num_gte(),
            _ => panic!("invalid numcmp keyword {}", op),
        });
        Ok(())
    }
    fn error_at(&self, tok: &Token, reason: String) -> Error {
        SyntaxError::new(tok.pos.clone(), reason).into()
    }
    fn error(&self, reason: String) -> Error {
        self.error_at(&self.peek, reason)
    }
    fn end(mut self) -> FunctionRef {
        compiler!(self).end()
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
