use std::{ops::{Deref, DerefMut}, rc::Rc};

use num_bigint::BigInt;

#[derive(Clone, Copy)]
pub struct Position {
    pub row: usize,
    pub col: usize
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Self {
            row,
            col
        }
    }
}

pub struct WithLoc<T> {
    pub loc: Location,
    pub inner: T
}

impl<T> Deref for WithLoc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for WithLoc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Clone)]
pub struct ConcreteLocation {
    pub path: Rc<String>,
    pub start: Position,
    pub end: Position
}

impl ConcreteLocation {
    pub fn new(path: Rc<String>, start: Position, end: Position) -> Self {
        Self {
            path,
            start,
            end
        }
    }
}

#[derive(Clone)]
pub enum Location {
    ConcreteLocation(ConcreteLocation),
    Index(usize, usize)
}

impl Location {
    pub fn to(&self, other: &Location) -> Self {
        match (self, other) {
            (Location::ConcreteLocation(ConcreteLocation { path, start, .. }),
            Location::ConcreteLocation(ConcreteLocation { end, .. }))
            => Location::ConcreteLocation(ConcreteLocation::new(path.clone(), *start, *end)),
            (Location::Index(start, _), Location::Index(_, end))
            => Location::Index(*start, *end),
            _ => panic!("aaaaaaaaa")
        }
    }
}

pub struct Program {
    pub loc: Location,
    pub struct_defs: Vec<StructDef>,
    pub impls: Vec<Impl>,
    pub func_defs: Vec<FuncDef>
}

pub struct FuncDef {
    pub loc: Location,
    pub constant: Option<Location>,
    pub pure: Option<WithLoc<bool>>,
    pub inline: Option<Location>,
    pub name: WithLoc<String>,
    pub type_parameters: Vec<TypeParameter>,
    pub parameters: Vec<Pattern>,
    pub return_type: Option<TypeExpression>,
    pub body: Expression
}

pub struct Impl {
    pub loc: Location,
    pub type_params: Vec<TypeParameter>,
    pub ttype: TypeExpression,
    pub methods: Vec<Method>,
    pub conditionals: Vec<MethodConditional>
}

pub struct MethodConditional {
    pub loc: Location,
    pub conds: Vec<WhereGuard>,
    pub methods: Vec<Method>
}

pub struct StructDef {
    pub loc: Location,
    pub name: WithLoc<String>,
    pub type_params: Vec<TypeParameter>,
    pub where_guards: Vec<WhereGuard>,
    pub conditionals: Vec<MemberConditional>,
    pub inits: Vec<Method>
}

pub struct Field {
    pub loc: Location,
    pub name: WithLoc<String>,
    pub ttype: TypeExpression
}

pub struct Method {
    pub loc: Location,
    pub sstatic: Option<Location>,
    pub inline: Option<Location>,
    pub name: WithLoc<String>,
    pub type_params: Vec<TypeParameter>,
    pub params: Vec<Pattern>,
    pub return_type: Option<TypeExpression>,
    pub body: Expression
}

pub struct MemberConditional {
    pub loc: Location,
    pub conds: Vec<WhereGuard>,
    pub fields: Vec<Field>,
    pub inits: Vec<Method>
}

pub struct WhereGuard {
    pub loc: Location,
    pub kind: WhereGuardKind
}

pub enum WhereGuardKind {
    TypeEqual { left: TypeExpression, right: TypeExpression },
    TypeImplements { left: TypeExpression, right: TypeExpression },
    ConstTypeComparison { left: ConstTypeExpression, op: ConstTypeCompOp, right: ConstTypeExpression },
    For { var: WithLoc<String>, iter: WithLoc<String>, guards: Vec<WhereGuard> }
}

pub struct ConstTypeExpression {
    pub loc: Location,
    pub kind: ConstTypeExpressionKind
}

pub enum ConstTypeExpressionKind {
    Type(TypeExpression),
    Length(TypeExpression),
    Literal(Expression)
}

pub enum ConstTypeCompOp {
    Eq,
    Neq,
    Gt,
    Gteq,
    Lt,
    Lteq
}

pub struct TypeParameter {
    pub loc: Location,
    pub kind: TypeParameterKind
}

pub enum TypeParameterKind {
    Type { is_splat: bool, name: WithLoc<String> },
    Const { name: WithLoc<String>, ttype: TypeExpression }
}

pub struct TypeExpression {
    pub loc: Location,
    pub kind: TypeExpressionKind
}

pub enum TypeExpressionKind {
    Infer,
    Ident { name: WithLoc<String>, args: Vec<TypeExpression> },
    Tuple(Vec<TypeExpression>),
    Splat(WithLoc<String>),
    ConstExpr(Box<Expression>)
}

pub struct Expression {
    pub loc: Location,
    pub kind: ExpressionKind
}

pub enum ExpressionKind {
    Block { stmnts: Vec<Statement>, expr: Option<Box<Expression>> },
    If { cond: Box<Expression>, body: Box<Expression>, elifs: Vec<(Expression, Expression)>, eelse: Option<Box<Expression>> },
    Binary { left: Box<Expression>, op: BinaryOp, right: Box<Expression> },
    Unary { expr: Box<Expression>, op: UnaryOp },
    PathAccess { expr: Box<Expression>, path: WithLoc<String> },
    MethodCall { receiver: Box<Expression>, name: WithLoc<String>, type_args: Vec<TypeExpression>, args: Vec<Expression> },
    FunctionCall { func: Box<Expression>, type_args: Vec<TypeExpression>, args: Vec<Expression> },
    TypeCreation { ttype: TypeExpression, type_args: Vec<TypeExpression>, args: Vec<Expression> },
    Match { expr: Box<Expression>, arms: Vec<MatchArm> },

    Splat(Box<Expression>),
    Tuple(Vec<Expression>),
    Identifier(String),
    String(String),
    Char(char),
    Integer(BigInt),
    Float(f64),
    Bool(bool),
    None
}

pub struct MatchArm {
    pub loc: Location,
    pub pattern: Pattern,
    pub expr: Expression
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Rem,
    Eq,
    Neq,
    Gt,
    Gteq,
    Lt,
    Lteq,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight
}

pub enum UnaryOp {
    Neg,
    Not,
    Deref,
    AddrOf
}

pub struct Statement {
    pub loc: Location,
    pub kind: StatementKind
}

pub enum StatementKind {
    Let { mutable: bool, pattern: Pattern, expr: Expression },
    Assign { path: Expression, expr: Expression },
    For { pattern: Pattern, iter: Expression, body: Expression },
    Loop(Expression),
    Break(Option<Expression>),
    Continue,
    Return(Option<Expression>),
    Expression(Expression),
    Panic(Expression)
}

pub struct Pattern {
    pub loc: Location,
    pub kind: PatternKind
}

pub enum PatternKind {
    Ignore,
    Identifier(WithLoc<String>),
    Typed { ttype: TypeExpression, inner: Box<Pattern> },
    Tuple(Vec<Pattern>),
    Rest(Option<WithLoc<String>>),
    Struct { ttype: TypeExpression, fields: Vec<StructFieldPattern>, ignore_rest: bool }
}

pub struct StructFieldPattern {
    pub loc: Location,
    pub field: WithLoc<String>,
    pub inner: Pattern
}
