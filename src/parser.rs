use crate::ast::{Expression, FuncDef, Impl, StructDef, TypeExpression, WithLoc};

enum ProgramMember {
    Struct(StructDef),
    Func(FuncDef),
    Impl(Impl)
}

peg::parser! {
    pub grammar parser() for str {
        use crate::ast::*;
        use num_bigint::BigInt;
        
        rule traced<T>(e: rule<T>) -> T =
        &(input:$([_]*) {
            #[cfg(feature = "trace")]
            println!("[PEG_INPUT_START]\n{}\n[PEG_TRACE_START]", input);
        })
        e:e()? {?
            #[cfg(feature = "trace")]
            println!("[PEG_TRACE_STOP]");
            e.ok_or("")
        }

        rule eol() = "\n"

        rule _() = (quiet!{"#--" (!"--#" [_])*} "--#" / quiet!{ c:$([_]) {? match c.chars().next().unwrap() {
            '\n' => Err("space"),
            c if c.is_whitespace() => Ok(()),
            c => Err("space")
        } } })*
        rule __() = (quiet!{"#--" (!"--#" [_])*} "--#" / quiet!{ c:$([_]) {? if c.chars().next().unwrap().is_whitespace() { Ok(()) } else { Err("space") } } })*

        rule hexdigit() -> char = c:$(['0'..='9'|'a'..='f'|'A'..='F']) { c.chars().next().unwrap() }

        rule escaped_char() -> char = "\\" c:(
            c:$(['\\'|'"'|'\''|'n'|'t'|'r']) { match c.chars().next().unwrap() {
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                _ => unreachable!()
            } }
            / ['x'|'X'] a:hexdigit() b:hexdigit() {
                // Safety: argument to from_u32_unchecked cannot be higher than 255, and the function is safe
                // for all values 0..=255
                unsafe { char::from_u32_unchecked(a.to_digit(16).unwrap() as u32 * 16 + b.to_digit(16).unwrap()) }
            }
            / ['u'|'U'] "{" xs:hexdigit()*<1,6> "}" { char::from_u32(xs.into_iter().fold(0, |a, c| a * 16 + c.to_digit(16).unwrap())).unwrap() }
        ) { c }

        rule string() -> WithLoc<String>
            = s:position!() "\"" cs:(escaped_char() / !"\"" c:$([_]) { c.chars().next().unwrap() })* "\"" e:position!()
            {
                WithLoc { loc: Location::Index(s, e), inner: cs.into_iter().collect() }
            }
        
        rule char() -> WithLoc<char>
            = s:position!() "'" c:(escaped_char() / !"'" c:$([_]) { c.chars().next().unwrap() }) "'" e:position!()
            { WithLoc { loc: Location::Index(s, e), inner: c } }
        
        rule int() -> WithLoc<BigInt>
            = s:position!() dd:("0" dd:(
                ['b'|'B'] dd:$(['0'|'1']+) { BigInt::parse_bytes(&dd.bytes().collect::<Vec<_>>(), 2).unwrap() }
                / ['x'|'X'] dd:$(['0'..='9'|'a'..='f'|'A'..='F']+) { BigInt::parse_bytes(&dd.bytes().collect::<Vec<_>>(), 16).unwrap() }
            ) { dd } / dd:$("-"? ['0'..='9']+) { BigInt::parse_bytes(&dd.bytes().collect::<Vec<_>>(), 16).unwrap() }) e:position!()
            { WithLoc { loc: Location::Index(s, e), inner: dd } }
        
        rule float() -> WithLoc<f64>
            = s:position!() dd:$("-"? ['0'..='9']+ "." ['0'..='9'] (['e'|'E'] ['0'..='9']+)?) e:position!()
            { WithLoc { loc: Location::Index(s, e), inner: dd.parse().unwrap() } }
        
        rule bool() -> WithLoc<bool>
            = s:position!() v:("true" { true } / "false" { false }) e:position!()
            { WithLoc { loc: Location::Index(s, e), inner: v } }
        
        rule none() -> Location
            = s:position!() "none" e:position!()
            { Location::Index(s, e) }

        rule raw_ident() -> WithLoc<String>
            = s:position!()
                cs:$((c:$([_]) {? let c = c.chars().next().unwrap(); if c.is_alphabetic() || c == '_' { Ok(c) } else { Err("valid identifier start") }})
                (c:$([_]) {? let c = c.chars().next().unwrap(); if c.is_alphanumeric() || c == '_' { Ok(c) } else { Err("valid identifier contiuer") } })*)
            e:position!()
            { WithLoc { loc: Location::Index(s, e), inner: cs.to_string() } }
        
        rule ident() -> WithLoc<String> = i:raw_ident() {? match i.inner.as_str() {
            "fn" | "const" | "pure" | "impure" | "inline" | "where" | "static"
            | "let" | "mut"
            | "if" | "else" | "for" | "in" | "loop" | "match"
            | "struct" | "interface"
            | "break" | "continue" | "return"
            | "panic" | "none" | "pass" | "_" | "and" | "or"
            //| "init" | "deinit"
            => Err("identifier"),
            _ => Ok(i)
        } } / expected!("identifier") 
        
        rule key(k: &'static str) -> Location
            = quiet!{i:raw_ident()
            {? if i.inner == k { Ok(i.loc) } else { Err(k) } }} / [_] {? Err(k)} 

        pub rule program_traced() -> Program = traced(<program()>)
        
        pub rule program() -> Program =
            sp:position!()
            (_ eol())*
            m:(
                s:struct_def() { ProgramMember::Struct(s) }
                / f:func_def() { ProgramMember::Func(f) }
                / f:for_block() { ProgramMember::Impl(f) }
            )**__
            _ ![_]
            ep:position!()
        { 
            let mut s = vec![];
            let mut f = vec![];
            let mut i = vec![];
            for m in m {
                match m {
                    ProgramMember::Struct(v) => s.push(v),
                    ProgramMember::Func(v) => f.push(v),
                    ProgramMember::Impl(v) => i.push(v),
                }
            }
            Program {
                loc: Location::Index(sp, ep),
                struct_defs: s,
                impls: i,
                func_defs: f
            }
        }

        rule func_def() -> FuncDef =
            s:position!()
            constant:key("const")? _ pure:(l:key("pure") { WithLoc { loc: l, inner: true } }/l:key("impure") { WithLoc { loc: l, inner: false } })? _ inline:key("inline")? _
            key("fn") _ name:ident() _ t:type_params()? _ "(" __ pp:(pp:pattern()**<1,>(__ "," __) __ "," { pp })? __ ")"
            w:(__ key("where") __ w:(w:where_guard()**<1,>(__ "," __) __ "," { w })? { w })? return_type:(__ "->" __ t:type_expr() { t })? __ body:stmnt_block()
            e:position!()
        { FuncDef {
            loc: Location::Index(s, e),
            constant,
            pure,
            inline,
            name,
            type_parameters: t.unwrap_or_default(),
            parameters: pp.unwrap_or_default(),
            return_type,
            body
        } }

        rule for_block() -> Impl =
            s:position!()
            constant:key("for") _ t:type_params()? _ ttype:type_expr() __ "{" __
            methods:method_def()**__ __
            conditionals:for_where()**__ __
            __ "}"
            e:position!()
        { Impl { loc: Location::Index(s, e), type_params: t.unwrap_or_default(), ttype, methods, conditionals } }

        rule for_where() -> MethodConditional =
            s:position!()
            key("where") __ conds:(w:where_guard()**<1,>(__ "," __) __ "," { w })? __ "{" __
            methods:method_def()**__
            __ "}"
            e:position!()
        { MethodConditional { loc: Location::Index(s, e), conds: conds.unwrap_or_default(), methods } }

        rule struct_def() -> StructDef =
            s:position!()
            key("struct") _ name:ident() _ t:type_params()? __
            w:(key("where") __ w:(w:where_guard()**(__ "," __) __ ","? { w })? { w })? __ "{" __
            struct_field()**__ __
            i:init_def()**__ __
            d:deinit_def()**__ __
            c:struct_where()**__
            __ "}"
            e:position!()
        { StructDef {
            loc: Location::Index(s, e),
            name,
            type_params: t.unwrap_or_default(),
            where_guards: w.flatten().unwrap_or_default(),
            conditionals: c,
            inits: { let mut i = i; i.extend(d); i }
        } }

        rule init_def() -> Method =
            s:position!()
            inlin:key("inline")? _ ns:position!() ident() ne:position!() _ t:type_params()? _ "(" __ params:(params:pattern()**<1,>(__ "," __) __ ","? { params })? __ ")"
            __ (key("where") __ (guards:where_guard()**<1,>(__ "," __) __ ","?)?)? __ body:stmnt_block()
            e:position!()
        { Method {
            loc: Location::Index(s, e),
            sstatic: None,
            inline: None,
            name: WithLoc { loc: Location::Index(s, e), inner: "init".into() },
            type_params: t.unwrap_or_default(),
            params: params.unwrap_or_default(),
            return_type: None,
            body
        } }

        rule deinit_def() -> Method =
            s:position!()
            inline:key("inline")? _ ns:position!() key("deinit") ne:position!() _ "(" __ ")" __ body:stmnt_block()
            e:position!()
        { Method {
            loc: Location::Index(s, e),
            sstatic: None,
            inline: None,
            name: WithLoc { loc: Location::Index(ns, ne), inner: "deinit".into() },
            type_params: vec![],
            params: vec![],
            return_type: None,
            body
        } }

        rule struct_where() -> MemberConditional =
            s:position!()
            key("where") __ c:(w:where_guard()**<1,>(__ "," __) __ ","? { w })? __ "{" __
            i:init_def()**__ __
            d:deinit_def()**__ __
            fields:struct_field()**__ __
            __ "}"
            e:position!()
        { MemberConditional { loc: Location::Index(s, e), conds: c.unwrap_or_default(), fields, inits: {let mut i = i; i.extend(d); i} } }

        rule struct_field() -> Field
            = name:ident() _ "::" _ ttype:type_expr()
        { Field { loc: name.loc.to(&ttype.loc), name, ttype } }

        rule type_expr() -> TypeExpression =
            s:position!() k:(
                key("_") { TypeExpressionKind::Infer }
                / name:ident() _ args:type_args()? { TypeExpressionKind::Ident { name, args: args.unwrap_or_default() } }
                / "(" __ t:type_expr() __ tt:(("," __ t:type_expr() { t })* / "," { vec![] }) __ ")" { TypeExpressionKind::Tuple(std::iter::once(t).chain(tt.into_iter()).collect()) }
                / "*" _ name:ident() { TypeExpressionKind::Splat(name) }
                / "{" __ e:expr() __ "}" { TypeExpressionKind::ConstExpr(Box::new(e)) }
            ) e:position!()
        { TypeExpression { loc: Location::Index(s, e), kind: k } }

        rule type_args() -> Vec<TypeExpression> =
            "[" __ tt:type_expr()**<1,>(__ "," __) __ ","? __ "]" { tt }

        rule method_def() -> Method =
            s:position!()
            sstatic:key("static")? _ inline:key("inline")? _ key:("fn")? _ name:ident() _ g:"="? _ type_params:type_params()? _ "(" __ params:(params:pattern()**<1,>(__ "," __) __ ","? { params })? __ ")"
            __ return_type:("->" __ t:type_expr() { t })? __ (key("where") __ (guards:where_guard()**<1,>(__ "," __) __ ","?)?)? __ body:stmnt_block()
            e:position!()
        { Method {
            loc: Location::Index(s, e),
            sstatic,
            inline,
            name: if g.is_some() { WithLoc { loc: name.loc, inner: format!("{}=", name.inner) } } else { name },
            type_params: type_params.unwrap_or_default(),
            params: params.unwrap_or_default(),
            return_type,
            body
        } }

        rule stmnt_block() -> Expression =
            s:position!()
            "{" __
            ss:(
                s:stmnt() _ sc:(eol() { false }/";" { true }) __
                { (s, sc) }
            )*
            __ "}"
            e:position!()
        {
            let mut ss = ss;
            let last = if ss.len() > 0 {
                if let (Statement { kind: StatementKind::Expression(_), .. }, false) = &ss[ss.len() - 1] {
                    Some(match ss.pop().unwrap().0.kind {
                        StatementKind::Expression(e) => e,
                        _ => unreachable!()
                    })
                } else {
                    None
                }
            } else {
                None
            };
            Expression { loc: Location::Index(s, e), kind: ExpressionKind::Block { stmnts: ss.into_iter().map(|(s, _)| s).collect(), expr: last.map(|e| Box::new(e)) } }
        }

        rule stmnt() -> Statement =
            s:position!()
            kind:(
                key("let") _ m:key("mut")? _ p:pattern() _ "=" _ expr:expr() { StatementKind::Let { mutable: m.is_some(), pattern: p, expr } }
                / key("for") _ pattern:pattern() _ key("in") _ iter:expr() __ body:stmnt_block()
                    { StatementKind::For { pattern, iter, body } }
                / key("loop") __ e:stmnt_block() { StatementKind::Loop(e) }
                / key("break") e:(_ e:expr() { e })? { StatementKind::Break(e) }
                / key("continue") { StatementKind::Continue }
                / key("return") e:(_ e:expr() { e })? { StatementKind::Return(e) }
                / key("panic") _ e:expr() { StatementKind::Panic(e) }
                / e:expr() e2:(_ "=" _ e2:expr() { e2 })? { match e2 {
                    Some(e2) => StatementKind::Assign { path: e, expr: e2 },
                    None => StatementKind::Expression(e)
                } }
            )
            e:position!()
        { Statement { loc: Location::Index(s, e), kind } }

        rule pattern() -> Pattern =
            p:pattern_() t:(_ "::" _ t:type_expr() { t })?
        { match t {
            None => p,
            Some(t) => Pattern { loc: p.loc.to(&t.loc), kind: PatternKind::Typed { ttype: t, inner: Box::new(p) } }
        } }

        rule pattern_() -> Pattern =
            s:position!()
            kind:(
                "_" { PatternKind::Ignore }
                / name:ident() { PatternKind::Identifier(name) }
                / "(" __ pp:(p:pattern()**<1,>(__ "," __) __ ","? { p })? __ ")" { PatternKind::Tuple(pp.unwrap_or_default()) }
                / ".." name:(_ name:ident() { name })? { PatternKind::Rest(name) }
                / ttype:type_expr() __ "{" __ f:(f:(n:ident() _ ":" _ p:pattern() { StructFieldPattern { loc: n.loc.to(&p.loc), field: n, inner: p } })**<1,>(__ "," __) __ r:("," __ ".." { true } / ","? { false }) { (f, r) })?  __ "}"
                    { let f = f.unwrap_or_default(); PatternKind::Struct { ttype, fields: f.0, ignore_rest: f.1 } }
            )
            e:position!()
        { Pattern { loc: Location::Index(s, e), kind } }

        rule type_params() -> Vec<TypeParameter> =
            "[" __  tt:type_param()**<1,>(__ "," __) __ ","? __ "]" { tt }

        rule type_param() -> TypeParameter =
            s:position!()
            kind:(
                key("const") _ name:ident() _ "::" _ ttype:type_expr()
                { TypeParameterKind::Const { name, ttype } }
                / s:"*"? name:ident()
                { TypeParameterKind::Type { is_splat: s.is_some(), name } }
            )
            e:position!()
        { TypeParameter { loc: Location::Index(s, e), kind } }

        rule where_guard() -> WhereGuard =
            s:position!()
            kind:(
                "{" __ left:const_type_expr() __ op:$("=="/"!="/">="/"<="/">"/"<") __ right:const_type_expr() __ "}"
                    { WhereGuardKind::ConstTypeComparison { left, op: match op {
                        "==" => ConstTypeCompOp::Eq,
                        "!=" => ConstTypeCompOp::Neq,
                        ">" => ConstTypeCompOp::Gt,
                        ">=" => ConstTypeCompOp::Gteq,
                        "<" => ConstTypeCompOp::Lt,
                        "<=" => ConstTypeCompOp::Lteq,
                        _ => unreachable!()
                    }, right } }
                / key("for") _ var:ident() _ "in" _ "*" iter:ident() __ "{" __
                guards:where_guard()**__
                __ "}"
                { WhereGuardKind::For { var, iter, guards } }
                / left: type_expr() _ op:$("=="/"<:") _ right: type_expr() { match op {
                    "==" => WhereGuardKind::TypeEqual { left, right },
                    "<:" => WhereGuardKind::TypeImplements { left, right },
                    _ => unreachable!()
                } }
            )
            e:position!()
        { WhereGuard { loc: Location::Index(s, e), kind } }

        rule const_type_expr() -> ConstTypeExpression =
            s: position!()
            kind:(t:type_expr() k:(_ "." _ k:key("len") {k})? {if k.is_some() {
                ConstTypeExpressionKind::Type(t)
            } else {
                ConstTypeExpressionKind::Length(t)
            }}
            / l:(l:int() { (l.loc, ExpressionKind::Integer(l.inner)) }/l:bool() { (l.loc, ExpressionKind::Bool(l.inner)) }/l:char() { (l.loc, ExpressionKind::Char(l.inner)) })
              {ConstTypeExpressionKind::Literal(Expression { loc: l.0.clone(), kind: l.1 })})
            e: position!()
        { ConstTypeExpression { loc: Location::Index(s, e), kind } }

        /*
        
    1   () .() . @ & !
    2   -
    3   << >>       !
    4   &           ->
    5   |           ->
    6   ^           ->
    7   * / // %    ->
    8   - +         ->
    9   == !=       !
    a   > < >= <=   !
    b   and         ->
    c   or          ->

        */

        rule expr() -> Expression =
            expr_12()
        
        rule expr_12() -> Expression =
            e:expr_11() ee:(_ key("or") _ e:expr_11() { e })*
        {
            let mut e = e;
            for ee in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: BinaryOp::Or, right: Box::new(ee) } }
            }
            e
        }

        rule expr_11() -> Expression =
            e:expr_10() ee:(_ key("and") _ e:expr_10() { e })*
        {
            let mut e = e;
            for ee in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: BinaryOp::And, right: Box::new(ee) } }
            }
            e
        }

        rule expr_10() -> Expression =
            e:expr_9() e2:(_ op:$(">="/"<="/">"/"<") _ e2:expr_9() { (op, e2) })?
        {
            let mut e = e;
            if let Some((op, e2)) = e2 {
                e = Expression {
                    loc: e.loc.to(&e2.loc),
                    kind: ExpressionKind::Binary { left: Box::new(e), op: match op {
                        ">" => BinaryOp::Gt,
                        ">=" => BinaryOp::Gteq,
                        "<" => BinaryOp::Lt,
                        "<=" => BinaryOp::Lteq,
                        _ => unreachable!()
                    }, right: Box::new(e2) }
                };
            }
            e
        }

        rule expr_9() -> Expression =
            e:expr_8() e2:(_ op:$("=="/"!=") _ e2:expr_8() { (op, e2) })?
        {
            let mut e = e;
            if let Some((op, e2)) = e2 {
                e = Expression {
                    loc: e.loc.to(&e2.loc),
                    kind: ExpressionKind::Binary { left: Box::new(e), op: match op {
                        "==" => BinaryOp::Eq,
                        "!=" => BinaryOp::Neq,
                        _ => unreachable!()
                    }, right: Box::new(e2) }
                };
            }
            e
        }

        rule expr_8() -> Expression =
            e:expr_7() ee:(_ op:$(['+'|'-']) _ e:expr_7() { (op, e) })*
        {
            let mut e = e;
            for (op, ee) in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: match op {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    _ => unreachable!()
                }, right: Box::new(ee) } }
            }
            e
        }

        rule expr_7() -> Expression =
            e:expr_6() ee:(_ op:$("//" / ['*'|'/'|'%']) _ e:expr_6() { (op, e) })*
        {
            let mut e = e;
            for (op, ee) in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: match op {
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    "//" => BinaryOp::IntDiv,
                    "%" => BinaryOp::Rem,
                    _ => unreachable!()
                }, right: Box::new(ee) } }
            }
            e
        }
        
        rule expr_6() -> Expression =
            e:expr_5() ee:(_ "^" _ e:expr_5() { e })*
        {
            let mut e = e;
            for ee in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: BinaryOp::BitXor, right: Box::new(ee) } }
            }
            e
        }
        
        rule expr_5() -> Expression =
            e:expr_4() ee:(_ "|" _ e:expr_4() { e })*
        {
            let mut e = e;
            for ee in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: BinaryOp::BitOr, right: Box::new(ee) } }
            }
            e
        }
        
        rule expr_4() -> Expression =
            e:expr_3() ee:(_ "&" _ e:expr_3() { e })*
        {
            let mut e = e;
            for ee in ee {
                e = Expression { loc: e.loc.to(&ee.loc), kind: ExpressionKind::Binary { left: Box::new(e), op: BinaryOp::BitOr, right: Box::new(ee) } }
            }
            e
        }

        rule expr_3() -> Expression =
            e:expr_2() e2:(_ op:$("<<"/">>") _ e2:expr_2() { (op, e2) })?
        {
            let mut e = e;
            if let Some((op, e2)) = e2 {
                e = Expression {
                    loc: e.loc.to(&e2.loc),
                    kind: ExpressionKind::Binary { left: Box::new(e), op: match op {
                        "<<" => BinaryOp::BitShiftLeft,
                        ">>" => BinaryOp::BitShiftRight,
                        _ => unreachable!()
                    }, right: Box::new(e2) }
                };
            }
            e
        }

        rule expr_2() -> Expression =
            m:(s:position!() "-" _ { s })* e:expr_1()
        {
            let mut e = e;
            for s in m {
                e = Expression {
                    loc: Location::Index(s, s).to(&e.loc),
                    kind: ExpressionKind::Unary { expr: Box::new(e), op: UnaryOp::Neg }
                }
            }
            e
        }

        rule expr_1() -> Expression =
            e:expr_0() ee:(_ s:(
                t:type_args()? _ "(" __ args:expr_list()? __ ")" { Suffix::FuncCall(t.unwrap_or_default(), args.unwrap_or_default()) }
                / "." _ name:ident()? _ n:position!() t:type_args()? _ "(" __ args:expr_list()? __ ")" { Suffix::MethodCall(
                    name.unwrap_or_else(|| WithLoc { loc: Location::Index(n, n), inner: "apply".into() }),
                    t.unwrap_or_default(),
                    args.unwrap_or_default()
                ) }
                / "." _ name:ident() { Suffix::Field(name) }
                / "@" { Suffix::Deref }
                / "&" { Suffix::AddrOf }
                / "!" { Suffix::Not }
            ) e:position!() { (s, e) })*
        {
            let mut e = e;
            for (s, e2) in ee {
                e = Expression {
                    loc: e.loc.to(&Location::Index(0, e2)),
                    kind: match s {
                        Suffix::FuncCall(type_args, args) => ExpressionKind::FunctionCall { func: Box::new(e), type_args, args },
                        Suffix::MethodCall(name, type_args, args) => ExpressionKind::MethodCall { receiver: Box::new(e), name, type_args, args },
                        Suffix::Field(name) => ExpressionKind::PathAccess { expr: Box::new(e), path: name },
                        Suffix::Deref => ExpressionKind::Unary { expr: Box::new(e), op: UnaryOp::Deref },
                        Suffix::AddrOf => ExpressionKind::Unary { expr: Box::new(e), op: UnaryOp::AddrOf },
                        Suffix::Not => ExpressionKind::Unary { expr: Box::new(e), op: UnaryOp::Not }
                    }
                };
            }
            e
        }

        rule expr_0() -> Expression =
            stmnt_block()
            / s:position!() "(" __ ex:expr() __ ee:("," __ ee:expr_list()? { ee })? __ ")" e:position!()
            {
                match ee {
                    Some(ee) => {
                        let mut ee = ee.unwrap_or_default();
                        ee.insert(0, ex);
                        Expression { loc: Location::Index(s, e), kind: ExpressionKind::Tuple(ee) }
                    },
                    None => ex
                }
            }
            / s:position!() key("if") _ c:expr() __ b:stmnt_block() ei:(__ key("else") __ key("if") __ c:expr() __ b:stmnt_block() { (c, b) })*
              el:(__ key("else") __ b:stmnt_block() {b})? e:position!()
            {
                Expression {
                    loc: Location::Index(s, e),
                    kind: ExpressionKind::If { cond: Box::new(c), body: Box::new(b), elifs: ei, eelse: el.map(|e| Box::new(e)) }
                }
            }
            / s:position!() key("new") _ ttype:type_expr() _ it:type_args()? _ "(" __ args:expr_list()? __ ")" e:position!()
            { Expression { loc: Location::Index(s, e), kind: ExpressionKind::TypeCreation {
                ttype, type_args: it.unwrap_or_default(), args: args.unwrap_or_default()
            } }}
            / s:position!() key("match") _ expr:expr() __ "{" __
                arms:(s:position!() p:pattern() __ "=>" __ ex:expr() e:position!() __ { MatchArm {
                    loc: Location::Index(s, e), pattern: p, expr: ex
                } })*
            __ "}" e:position!()
            { Expression { loc: Location::Index(s, e), kind: ExpressionKind::Match {
                expr: Box::new(expr), arms
            } } }
            / i:ident() { Expression { loc: i.loc, kind: ExpressionKind::Identifier(i.inner) } }
            / s:string() { Expression { loc: s.loc, kind: ExpressionKind::String(s.inner) } }
            / s:char() { Expression { loc: s.loc, kind: ExpressionKind::Char(s.inner) } }
            / s:bool() { Expression { loc: s.loc, kind: ExpressionKind::Bool(s.inner) } }
            / s:int() { Expression { loc: s.loc, kind: ExpressionKind::Integer(s.inner) } }
            / s:float() { Expression { loc: s.loc, kind: ExpressionKind::Float(s.inner) } }
            / s:none() { Expression { loc: s, kind: ExpressionKind::None } }

        rule expr_list() -> Vec<Expression> =
            e:expr()**<1,>(__ "," __) (__ ",")?
        { e }
    } 
}

enum Suffix {
    FuncCall(Vec<TypeExpression>, Vec<Expression>),
    MethodCall(WithLoc<String>, Vec<TypeExpression>, Vec<Expression>),
    Field(WithLoc<String>),
    Deref,
    AddrOf,
    Not
}