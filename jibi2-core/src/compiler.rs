use std::convert::TryInto;
use std::fmt;
use std::rc::Rc;

use crate::error::{Error, Result};
use crate::instruction::{ins, make_long_operands, AnyInstruction, Instruction, OP};
use crate::object::*;
use crate::reader::parser::{Parser, SyntaxError};
use crate::reader::{PositionTag, TokenProducer, Tokenizer};
use crate::vm::{CallFrame, VM};

pub fn compile_tokens(name: &str, producer: Box<dyn TokenProducer>) -> Result<FunctionRef> {
    let parser = Parser::new(producer);
    let mut compiler = Compiler::new(Rc::new(name.to_string()));
    compiler.compile(parser)
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

#[derive(Debug)]
pub struct Upvalue {
    name: String,
    var: Variable,
}

impl Upvalue {
    pub fn new(name: String, var: Variable) -> Self {
        Self { name, var }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Global,
    Local(usize),
    Upvalue(usize),
}

pub struct FunctionCompiler {
    function: FunctionRef,
    scope_depth: i32,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    lists: Vec<usize>,
    pos: PositionTag,
    enclosing: Option<Box<FunctionCompiler>>,
}

impl FunctionCompiler {
    pub fn new(name: Rc<String>) -> Self {
        let function = Function::new(name);
        Self {
            function: function.into_ref(),
            scope_depth: 0,
            locals: vec![Local::new("".to_string(), 0)],
            upvalues: vec![],
            lists: vec![],
            pos: PositionTag::new("", 1, 0),
            enclosing: None,
        }
    }
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
    pub fn enclose(&mut self, compiler: Box<FunctionCompiler>) {
        self.enclosing = Some(compiler);
    }
    fn offset(&self) -> usize {
        self.function.borrow().code.count()
    }
    fn emit_instruction<I: Into<AnyInstruction>>(&mut self, ins: I) {
        self.function
            .borrow_mut()
            .code
            .write_instruction(ins, self.pos.lineno);
    }
    fn add_constant(&mut self, val: Object) -> usize {
        self.function.borrow_mut().code.add_constant(val)
    }
    fn emit_constant(&mut self, val: Object) {
        self.function
            .borrow_mut()
            .code
            .write_constant(val, self.pos.lineno);
    }
    fn emit_return(&mut self) {
        let mut function = self.function.borrow_mut();
        let codelen = function.code.code.len();
        let instructions = AnyInstruction::read_all(&function.code.code[..], 0, codelen);

        let tail_call = self.enclosing.is_some()
            && !instructions.is_empty()
            && instructions.last().unwrap().op() == OP::CALL;
        if tail_call {
            function.code.code[codelen - 2] = OP::TAIL_CALL;
        }
        // Emit an OP::RETURN even if it's a tail call. The OP::RETURN will never be
        // executed after OP::TAIL_CALL since OP::TAIL_CALL rewrites the current call
        // frame, but if and cond may have jumps that land right after the OP::TAIL_CALL,
        // in which case we still need an OP::RETURN here.
        // TODO?: keep track of whether there is any such jump, only emit OP::RETURN
        // for tail calls if there is.
        function
            .code
            .write_instruction(ins::r#return(), self.pos.lineno);
    }
    fn emit_jump(&mut self, op: u8) -> usize {
        self.function
            .borrow_mut()
            .code
            .write_instruction(Instruction::new(op, [0xff, 0xff]), self.pos.lineno)
    }
    fn patch_jump(&mut self, jmp: usize) {
        let loc = self.offset() - jmp - 3;
        if loc > u16::MAX as usize {
            panic!("jump offset too large");
        }
        let [b1, b2] = make_long_operands(loc.try_into().unwrap());
        self.function.borrow_mut().code.write_at(jmp + 1, b1);
        self.function.borrow_mut().code.write_at(jmp + 2, b2);
    }
    fn declare_variable(&mut self, name: String) -> Result<Variable> {
        if self.scope_depth == 0 {
            Ok(Variable::Global)
        } else {
            if self.locals.len() >= u16::MAX.into() {
                panic!("too many local variables")
            }
            for local in self.locals.iter().rev() {
                if local.depth < self.scope_depth {
                    break;
                }
                if local.name == name {
                    return Err(SyntaxError::new(
                        self.pos.clone(),
                        format!("cannot re-declare local variable {}", name),
                    )
                    .into());
                }
            }
            self.locals.push(Local::new(name, self.scope_depth));
            Ok(Variable::Local(self.locals.len() - 1))
        }
    }
    fn define_variable(&mut self, name: String, var: Variable) -> Result<()> {
        match var {
            // Global: store in globals hashmap
            Variable::Global => {
                self.emit_constant(Object::Symbol(Rc::new(name)));
                self.emit_instruction(ins::def_global());
                Ok(())
            }
            // Local: variable is already on the stack, nothing to do
            Variable::Local(_) => Ok(()),
            Variable::Upvalue(_) => panic!("defining an upvalue?"),
        }
    }
    fn resolve_upvalue(&mut self, name: &str) -> Option<Variable> {
        match &mut self.enclosing {
            Some(enclosing) => {
                let var = match enclosing.resolve_local(name) {
                    Some(Variable::Local(i)) => Variable::Local(i),
                    Some(Variable::Upvalue(i)) => Variable::Upvalue(i),
                    _ => return None,
                };
                Some(Variable::Upvalue(self.add_upvalue(name, var)))
            }
            None => None,
        }
    }
    fn add_upvalue(&mut self, name: &str, var: Variable) -> usize {
        for (i, uv) in self.upvalues.iter().enumerate() {
            if uv.name == name && uv.var == var {
                return i;
            }
        }
        self.function.borrow_mut().upvalues.push(var.clone());
        self.upvalues.push(Upvalue::new(name.to_string(), var));
        self.upvalues.len() - 1
    }
    fn resolve_local(&mut self, name: &str) -> Option<Variable> {
        if !self.locals.is_empty() {
            for i in (0..self.locals.len()).rev() {
                if self.locals[i].name == name {
                    return Some(Variable::Local(i));
                }
            }
        }
        self.resolve_upvalue(name)
    }
    fn resolve_variable(&mut self, name: &str) -> Variable {
        match self.resolve_local(name) {
            Some(Variable::Local(i)) => Variable::Local(i),
            Some(Variable::Upvalue(i)) => Variable::Upvalue(i),
            _ => Variable::Global,
        }
    }
    fn get_variable(&mut self, name: String) -> Result<()> {
        let var = self.resolve_variable(&name);
        match var {
            Variable::Global => {
                self.emit_constant(Object::Symbol(Rc::new(name)));
                self.emit_instruction(ins::get_global());
            }
            Variable::Local(n) => {
                self.emit_instruction(ins::get_local(n));
            }
            Variable::Upvalue(n) => {
                self.emit_instruction(ins::get_upvalue(n));
            }
        }
        Ok(())
    }
    fn set_variable(&mut self, name: String) -> Result<()> {
        let var = self.resolve_variable(&name);
        match var {
            Variable::Global => {
                self.emit_constant(Object::Symbol(Rc::new(name)));
                self.emit_instruction(ins::set_global());
            }
            Variable::Local(n) => {
                self.emit_instruction(ins::set_local(n));
            }
            Variable::Upvalue(n) => {
                self.emit_instruction(ins::set_upvalue(n));
            }
        }
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }
    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        let mut popped: u8 = 0;
        while !self.locals.is_empty() && self.locals[self.locals.len() - 1].depth > self.scope_depth
        {
            self.locals.pop().unwrap();
            popped += 1;
            if popped == u8::MAX {
                self.emit_instruction(ins::pop_n(popped));
                popped = 0;
            }
        }
        if popped > 0 {
            self.emit_instruction(ins::pop_n(popped))
        }
    }
    fn begin_list(&mut self) {
        self.lists.push(self.offset());
    }
    fn end_list(&mut self) {
        let start = self.lists.pop().unwrap();
        // Pre-evaluate static expressions; if there is an expression containing only
        // constants, for example (+ 15 15), we can evaluate it at compile time
        // and just write the result in the compiled chunk
        if self.is_static_expr(start) {
            let res = self.evaluate(start);
            self.emit_constant(res);
        }
    }
    fn evaluate(&mut self, start: usize) -> Object {
        self.emit_instruction(ins::halt());

        #[cfg(debug_trace_compile)]
        self.function
            .borrow()
            .code
            .disassemble("static expression", start);

        let mut vm = VM::new();
        let closure = Closure::new(self.function.borrow().clone(), None).into_ref();
        vm.stack.push(Object::Closure(Rc::clone(&closure)));
        vm.frames.push(CallFrame::new(closure, start, 0));
        let res = vm.run().unwrap().unwrap();

        self.function.borrow_mut().code.erase(start);

        #[cfg(debug_trace_compile)]
        println!("evaluated to: {}", res);

        res
    }
    fn is_static_expr(&self, start: usize) -> bool {
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
    pub fn end(&mut self) -> (Option<Box<FunctionCompiler>>, FunctionRef) {
        self.emit_return();

        #[cfg(debug_trace_compile)]
        self.function
            .borrow()
            .code
            .disassemble(&format!("{}", self.function.borrow()), 0);

        let enclosing = self.enclosing.take();
        let function = std::mem::replace(
            &mut self.function,
            Function::new(Rc::new("".to_string())).into_ref(),
        );
        (enclosing, function)
    }
}

pub struct Compiler {
    compiler: Box<FunctionCompiler>,
    pos: PositionTag,
}

impl Compiler {
    pub fn new(name: Rc<String>) -> Self {
        Self {
            compiler: FunctionCompiler::new(name).boxed(),
            pos: PositionTag::new("", 1, 0),
        }
    }
    pub fn compile<I: Iterator<Item = Result<(Object, PositionTag)>>>(
        &mut self,
        forms: I,
    ) -> Result<FunctionRef> {
        let mut forms = forms.peekable();
        // Empty program, return nil
        if forms.peek().is_none() {
            self.compiler.emit_constant(Object::Nil);
        } else {
            while let Some(next) = forms.next() {
                let (sexpr, pos) = next?;
                self.compiler.pos = pos.clone();
                self.pos = pos;
                self.top_level_sexpr(&sexpr)?;
                if forms.peek().is_some() {
                    self.compiler.emit_instruction(ins::pop());
                }
            }
        }
        let (_, func) = self.compiler.end();
        Ok(func)
    }
    fn top_level_sexpr(&mut self, sexpr: &Object) -> Result<()> {
        if !self.sform_def(sexpr)? {
            self.sexpr(sexpr)?;
        }
        Ok(())
    }
    fn sform_def(&mut self, sexpr: &Object) -> Result<bool> {
        match sexpr {
            Object::Pair(p) => match p.car().as_symbol().ok() {
                Some("def") => {
                    self.def(p.cdr().as_pair()?)?;
                    self.compiler.emit_constant(Object::Nil);
                    Ok(true)
                }
                _ => Ok(false),
            },
            _ => Ok(false),
        }
    }
    fn def(&mut self, pair: PairRef) -> Result<()> {
        let [sym, val] = self.n_elements("def", pair)?;
        let sym = sym.as_symbol()?;
        let var = self.compiler.declare_variable(sym.to_owned())?;
        self.sexpr(&val)?;
        self.compiler.define_variable(sym.to_owned(), var)?;
        Ok(())
    }
    fn sexpr(&mut self, sexpr: &Object) -> Result<()> {
        match sexpr {
            Object::Pair(p) => self.list(Rc::clone(p)),
            _ => self.atom(sexpr),
        }
    }
    fn atom(&mut self, sexpr: &Object) -> Result<()> {
        match sexpr {
            Object::Symbol(s) => self.atom_symbol(s.to_string()),
            _ => {
                self.compiler.emit_constant(sexpr.clone());
                Ok(())
            }
        }
    }
    fn atom_symbol(&mut self, sym: String) -> Result<()> {
        match &sym[..] {
            "nil" => self.compiler.emit_constant(Object::Nil),
            "true" => self.compiler.emit_constant(Object::Bool(true)),
            "false" => self.compiler.emit_constant(Object::Bool(false)),
            _ => self.compiler.get_variable(sym)?,
        }
        Ok(())
    }
    fn list(&mut self, pair: PairRef) -> Result<()> {
        self.compiler.begin_list();
        if self.special_form(Rc::clone(&pair))? {
            self.compiler.end_list();
            return Ok(());
        }
        let items: Vec<Object> = pair.iter()?.collect();
        self.sexpr(&items[0])?;
        let mut n: u8 = 0;
        for arg in &items[1..] {
            self.sexpr(arg)?;
            n += 1;
        }
        self.compiler.emit_instruction(ins::apply(n));
        self.compiler.end_list();
        Ok(())
    }
    fn special_form(&mut self, pair: PairRef) -> Result<bool> {
        let rest = pair.cdr();
        match pair.car().as_symbol().ok() {
            Some("def") => {
                return Err(self.error("def not at top level or top of scope".to_string()))
            }
            Some("set!") => self.sform_set(rest.as_pair()?)?,
            Some("begin") => self.sform_begin(&rest)?,
            Some("let") => self.sform_let(&rest)?,
            Some("if") => self.sform_if(&rest)?,
            Some("cond") => self.sform_cond(&rest)?,
            Some("fn") => self.sform_fn(&rest)?,
            Some("equal?") => self.sform_simple::<_, 2>(rest.as_pair()?, "equal?", ins::equal)?,
            Some("print") => self.sform_simple::<_, 1>(rest.as_pair()?, "print", ins::print)?,
            Some("repr") => self.sform_simple::<_, 1>(rest.as_pair()?, "repr", ins::repr)?,
            Some("cons") => self.sform_simple::<_, 2>(rest.as_pair()?, "cons", ins::cons)?,
            Some("car") => self.sform_simple::<_, 1>(rest.as_pair()?, "car", ins::car)?,
            Some("cdr") => self.sform_simple::<_, 1>(rest.as_pair()?, "cdr", ins::cdr)?,
            Some("list") => self.sform_list(&rest)?,
            Some("=") => self.sform_simple::<_, 2>(rest.as_pair()?, "=", ins::num_eq)?,
            Some("!=") => self.sform_simple::<_, 2>(rest.as_pair()?, "!=", ins::num_neq)?,
            Some("<") => self.sform_simple::<_, 2>(rest.as_pair()?, "<", ins::num_lt)?,
            Some("<=") => self.sform_simple::<_, 2>(rest.as_pair()?, "<=", ins::num_lte)?,
            Some(">") => self.sform_simple::<_, 2>(rest.as_pair()?, ">", ins::num_gt)?,
            Some(">=") => self.sform_simple::<_, 2>(rest.as_pair()?, ">=", ins::num_gte)?,
            Some("+") => self.sform_var(rest.as_pair()?, ins::add)?,
            Some("-") => self.sform_var(rest.as_pair()?, ins::sub)?,
            Some("*") => self.sform_var(rest.as_pair()?, ins::mul)?,
            Some("/") => self.sform_var(rest.as_pair()?, ins::div)?,
            _ => return Ok(false),
        };
        Ok(true)
    }
    fn sform_set(&mut self, pair: PairRef) -> Result<()> {
        let [sym, val] = self.n_elements("set!", pair)?;
        self.sexpr(&val)?;
        self.compiler.set_variable(sym.as_symbol()?.to_string())?;
        self.compiler.emit_constant(Object::Nil);
        Ok(())
    }
    fn sform_simple<F: Fn() -> Instruction<0>, const N: usize>(
        &mut self,
        pair: PairRef,
        name: &str,
        insf: F,
    ) -> Result<()> {
        let args: [Object; N] = self.n_elements(name, pair)?;
        for arg in &args {
            self.sexpr(arg)?;
        }
        self.compiler.emit_instruction(insf());
        Ok(())
    }
    fn sform_var<F: Fn(usize) -> AnyInstruction>(&mut self, pair: PairRef, insf: F) -> Result<()> {
        let args: Vec<Object> = pair.iter()?.collect();
        for arg in args.iter() {
            self.sexpr(arg)?;
        }
        self.compiler.emit_instruction(insf(args.len()));
        Ok(())
    }
    fn sform_begin(&mut self, list: &Object) -> Result<()> {
        self.block(list)
    }
    fn sform_let(&mut self, list: &Object) -> Result<()> {
        self.begin_scope();
        let list = list.as_pair()?;
        let defs: Vec<Object> = list.car().iter_list()?.collect();
        for def in &defs {
            self.def(def.as_pair()?)?;
        }
        let exprs = list.cdr();
        self.block(&exprs)?;
        self.end_scope(defs.len());
        Ok(())
    }
    fn sform_if(&mut self, list: &Object) -> Result<()> {
        let start = self.compiler.offset();
        let [pred, then_expr, else_expr] = self.n_elements("if", list.as_pair()?)?;
        self.sexpr(&pred)?;
        // If the predicate is a static expression, don't bother with jumps, just
        // compile the correct branch
        if self.compiler.is_static_expr(start) {
            let pred = self.compiler.evaluate(start).as_bool()?;
            self.sexpr(&if pred { then_expr } else { else_expr })?;
        } else {
            let to_else = self.compiler.emit_jump(OP::JUMP_FALSE);
            self.compiler.emit_instruction(ins::pop());
            self.sexpr(&then_expr)?;
            let to_end = self.compiler.emit_jump(OP::JUMP);
            self.compiler.patch_jump(to_else);
            self.compiler.emit_instruction(ins::pop());
            self.sexpr(&else_expr)?;
            self.compiler.patch_jump(to_end);
        }
        Ok(())
    }
    fn sform_cond(&mut self, list: &Object) -> Result<()> {
        let conds: Vec<Object> = list.iter_list()?.collect();

        let mut jumps_to_end: Vec<usize> = vec![];
        let mut has_else_clause = false;
        for cond in &conds {
            let cond = cond.as_pair()?;
            let pred = cond.car();
            let body = cond.cdr();
            let start = self.compiler.offset();
            self.sexpr(&pred)?;
            if self.compiler.is_static_expr(start) {
                if self.compiler.evaluate(start).as_bool()? {
                    self.block(&body)?;
                }
                has_else_clause = true;
                break;
            } else {
                let jump_skip = self.compiler.emit_jump(OP::JUMP_FALSE);
                self.compiler.emit_instruction(ins::pop());
                self.block(&body)?;
                jumps_to_end.push(self.compiler.emit_jump(OP::JUMP));
                self.compiler.patch_jump(jump_skip);
                self.compiler.emit_instruction(ins::pop());
            }
        }
        // In case no condition is fulfilled
        if !has_else_clause {
            self.compiler.emit_constant(Object::Nil);
        }
        for jump in jumps_to_end {
            self.compiler.patch_jump(jump);
        }
        Ok(())
    }
    fn sform_fn(&mut self, list: &Object) -> Result<()> {
        let list = list.as_pair()?;
        let formals: Vec<String> = list
            .car()
            .iter_list()?
            .map(|obj| obj.as_symbol().map(|s| s.to_string()))
            .collect::<Result<Vec<String>>>()?;
        let body = list.cdr();

        let prev_compiler = std::mem::replace(
            &mut self.compiler,
            FunctionCompiler::new(Rc::new("unnamed".to_string())).boxed(),
        );
        self.compiler.pos = self.pos.clone();
        self.compiler.enclose(prev_compiler);
        self.compiler.begin_scope();
        self.compiler.function.borrow_mut().arity = formals.len();
        for name in formals {
            self.compiler.declare_variable(name.to_string())?;
        }
        self.block(&body)?;
        let (prev_compiler, func) = self.compiler.end();
        self.compiler = prev_compiler.unwrap();
        let n = self.compiler.add_constant(Object::Function(func));
        self.compiler.emit_instruction(ins::closure(n));
        Ok(())
    }
    fn sform_list(&mut self, list: &Object) -> Result<()> {
        let exprs: Vec<Object> = list.iter_list()?.collect();
        for expr in exprs.iter() {
            self.sexpr(expr)?;
        }
        self.compiler.emit_instruction(ins::list(exprs.len()));
        Ok(())
    }
    fn block(&mut self, list: &Object) -> Result<()> {
        // An empty block evaluates to nil
        let elements: Vec<Object> = list.iter_list()?.collect();
        let n_elems = elements.len();
        let mut n_defs = 0;
        let mut n_exprs = 0;
        self.begin_scope();
        // A block (begin, let, fn) evaluates to the result of the last expression.
        // So we pop the result of all the expressions except the last.
        for (i, element) in elements.iter().enumerate() {
            if self.sform_def(element)? {
                n_defs += 1;
                if i < n_elems - 1 {
                    self.compiler.emit_instruction(ins::pop());
                }
            } else {
                break;
            }
        }
        for (i, element) in elements.iter().enumerate().skip(n_defs) {
            self.sexpr(element)?;
            n_exprs += 1;
            if i < n_elems - 1 {
                self.compiler.emit_instruction(ins::pop());
            }
        }
        if n_exprs == 0 {
            self.compiler.emit_constant(Object::Nil);
        }
        self.end_scope(n_defs);
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.compiler.begin_scope();
    }
    fn end_scope(&mut self, n_defs: usize) {
        // If there are were local variable definitions, the top of stack will look like:
        //   <result>
        //   <local>
        //   ...
        //   <local>
        // So we save result to R0, end the scope, which pops locals, then push back
        // the result.
        if n_defs > 0 {
            self.compiler.emit_instruction(ins::pop_r0());
            self.compiler.end_scope();
            self.compiler.emit_instruction(ins::push_r0());
        // If there were no locals, ending the scope doesn't pop anything,
        // so we don't have to all that.
        } else {
            self.compiler.end_scope();
        }
    }

    fn error(&self, reason: String) -> Error {
        CompileError::new(self.pos.clone(), reason).into()
    }
    fn n_elements<const N: usize>(&self, name: &str, list: PairRef) -> Result<[Object; N]> {
        let elements: Vec<Object> = list.iter()?.collect();
        if elements.len() == N {
            Ok(elements.try_into().unwrap())
        } else {
            Err(self.error(format!("special form {} expected {} argument(s)", name, N)))
        }
    }
}

#[derive(Debug)]
pub struct CompileError {
    pub pos: PositionTag,
    pub reason: String,
}

impl CompileError {
    pub fn new(pos: PositionTag, reason: String) -> Self {
        Self { pos, reason }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "CompileError: {} at {}", self.reason, self.pos)
    }
}
