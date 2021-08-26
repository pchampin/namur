use super::ns::*;
use super::*;
use sophia_api::prefix::{Prefix, PrefixBox};
use sophia_iri::error::InvalidIri;
use sophia_iri::resolve::{IriParsed, Resolve};
use std::collections::HashSet;
use std::fmt;

pub const DEFAULT_BASE: &str = "http://example.org/";

pub fn parse(txt: &str, config: ParseConfig) -> ParseResult<Formula> {
    let mut p = Parser::new(txt, config);
    let f = p.parse_formula()?;
    if p.txt.is_empty() {
        Ok(f)
    } else {
        Err(p.err("Spurious content"))
    }
}

#[derive(Clone, Debug, Default)]
pub struct ParseConfig {
    pub base: Option<Iri>,
    pub prefixes: Vec<(PrefixBox, Iri)>,
    pub variable_offset: usize,
}

impl ParseConfig {
    pub fn new() -> ParseConfig {
        ParseConfig::default()
    }

    pub fn with_base(mut self, iri: Iri) -> Self {
        self.base = Some(iri);
        self
    }

    pub fn with_variable_offset(mut self, offset: usize) -> Self {
        self.variable_offset = offset;
        self
    }
}

struct Parser<'a> {
    txt: &'a str,
    config: ParseConfig,
    parsed_base: IriParsed<'a>,
    line: usize,
    column: usize,
    drafts: Vec<FormulaDraft>,
    next_var: usize,
    var_labels: HashSet<VarLabel>,
    term_stack: Vec<Term>,
}

impl<'a> Parser<'a> {
    fn new(txt: &'a str, mut config: ParseConfig) -> Parser<'a> {
        let parsed_base = if let Some(base) = &config.base {
            unsafe { std::mem::transmute(IriParsed::new(base).unwrap()) }
        } else {
            IriParsed::new(DEFAULT_BASE).unwrap()
        };
        if !config.prefixes.iter().any(|(pr, _)| pr.is_empty()) {
            config.prefixes.push((
                PrefixBox::new_unchecked("".into()),
                Iri::new_unchecked(parsed_base.resolve("").unwrap().to_string()),
            ))
        }
        let next_var = config.variable_offset;
        Parser {
            txt,
            config,
            parsed_base,
            line: 1,
            column: 0,
            drafts: vec![],
            next_var,
            var_labels: HashSet::new(),
            term_stack: vec![],
        }
    }

    fn parse_formula(&mut self) -> ParseResult<Formula> {
        self.drafts.push(FormulaDraft::default());
        self.ws();
        while matches!(self.txt.as_bytes().get(0), Some(c) if *c != b'}') {
            self.parse_statement()?;
            self.ws();
        }
        Ok(self.drafts.pop().unwrap().into())
    }

    /// Skip whitespace and comments
    fn ws(&mut self) {
        loop {
            self.consume_while(|c| c.is_ascii_whitespace());
            if self.try_consume(|c| c == '#') {
                self.consume_while(|c| c != '\n');
            } else {
                return;
            }
        }
    }

    fn parse_statement(&mut self) -> ParseResult {
        if self.try_consume(|c| c == '@') {
            if self.try_consume_str("base") {
                self.ws();
                let new_base = self.parse_iriref()?;
                self.set_base(new_base).map_err(|e| self.err(e))?;
            } else if self.try_consume_str("prefix") {
                self.ws();
                let pr = self.parse_prefix()?;
                self.ws();
                let ns = self.parse_iri()?;
                self.config.prefixes.push((pr.boxed(), ns));
            } else if self.try_consume_str("forSome") {
                // TODO if label is an IRI, check if it was used previously in this formula,
                // and do something (either complain or replace the Term::Iri with Term::Variable in existing triples)
                self.ws();
                let label = self.parse_var_label("_:")?;
                self.push_existential(label);
                self.ws();
                while self.try_consume(|c| c == ',') {
                    self.ws();
                    let label = self.parse_var_label("_:")?;
                    self.push_existential(label);
                    self.ws();
                }
            } else if self.try_consume_str("forAll") {
                self.ws();
                let label = self.parse_var_label("?")?;
                self.push_universal(label);
                self.ws();
                while self.try_consume(|c| c == ',') {
                    self.ws();
                    let label = self.parse_var_label("?")?;
                    self.push_universal(label);
                    self.ws();
                }
            } else {
                return Err(self.err("Unrecognized @ keyword"));
            }
            self.ws();
            self.expect(|c| c == '.')
        } else if self.try_consume_str_ci("BASE") {
            self.ws();
            let new_base = self.parse_iriref()?;
            self.set_base(new_base).map_err(|e| self.err(e))
        } else if self.try_consume_str_ci("PREFIX") {
            self.ws();
            let pr = self.parse_prefix()?;
            self.ws();
            let ns = self.parse_iri()?;
            self.config.prefixes.push((pr.boxed(), ns));
            Ok(())
        } else {
            self.parse_triples()
        }
    }

    fn parse_triples(&mut self) -> ParseResult {
        let s = self.parse_term()?;
        self.term_stack.push(s);
        self.parse_predicate_object_list()
    }

    fn parse_predicate(&mut self) -> ParseResult<Term> {
        match self.txt.as_bytes() {
            [b'a', ws, ..] if ws.is_ascii_whitespace() => {
                self.txt = &self.txt[1..];
                self.column += 1;
                Ok(rdf::type_.into())
            }
            [b'=', b'>', ws, ..] if ws.is_ascii_whitespace() => {
                self.txt = &self.txt[2..];
                self.column += 2;
                Ok(log::implies.into())
            }
            [b'<', b'=', ws, ..] if ws.is_ascii_whitespace() => {
                self.txt = &self.txt[2..];
                self.column += 2;
                todo!("predicate <=")
            }
            _ => self.parse_term(),
        }
    }

    fn parse_term(&mut self) -> ParseResult<Term> {
        match self.txt.as_bytes() {
            [b'<', ..] => self.parse_iri().map(|i| self.iri_to_term(i)),
            [b'"', ..] => {
                todo!("parse strings, language strings and datatyped literal")
            }
            [b'?', ..] => {
                self.forward("?");
                let name = self.parse_suffix();
                LocalId::new(name)
                    .map_err(|e| self.err(e))
                    .map(|n| self.quickvar_to_term(n))
            }
            [b'[', ..] => {
                todo!("parse anonymous bnodes")
            }
            [b'(', ..] => {
                self.forward("(");
                let mut v = vec![];
                self.ws();
                while !self.try_consume(|c| c == ')') {
                    v.push(self.parse_term()?);
                    self.ws();
                }
                Ok(v.into())
            }
            [b'{', ..] => {
                self.forward("{");
                self.ws();
                let f = self.parse_formula()?;
                self.expect(|c| c == '}')?;
                Ok(f.into())
            }
            [b'+', ..] | [b'-', ..] | [b'.', ..] => {
                todo!("parse integers and float")
            }
            [c, ..] if c.is_ascii_digit() => {
                todo!("parse integers and float")
            }
            [b'_', b':', ..] => {
                self.forward("_:");
                let name = self.parse_suffix();
                LocalId::new(name)
                    .map_err(|e| self.err(e))
                    .map(|n| self.bnode_to_term(n))
            }
            [b't', b'r', b'u', b'e', ..] => {
                self.forward("true");
                Ok(Literal::Boolean(true).into())
            }
            [b'f', b'a', b'l', b's', b'e', ..] => {
                self.forward("false");
                Ok(Literal::Boolean(false).into())
            }
            _ => self.parse_pname().map(|i| self.iri_to_term(i)),
        }
    }

    fn parse_predicate_object_list(&mut self) -> ParseResult {
        'p: loop {
            self.ws();
            let p = self.parse_predicate()?;
            self.term_stack.push(p);
            loop {
                self.ws();
                let o = self.parse_term()?;
                self.ws();
                if self.try_consume(|c| c == ',') {
                    let p = self.term_stack.last().unwrap().clone();
                    let s = self.term_stack[self.term_stack.len() - 2].clone();
                    self.push_triple(s, p, o);
                } else if self.try_consume(|c| c == ';') {
                    let p = self.term_stack.pop().unwrap();
                    let s = self.term_stack.last().unwrap().clone();
                    self.push_triple(s, p, o);
                    continue 'p;
                } else {
                    let p = self.term_stack.pop().unwrap();
                    let s = self.term_stack.pop().unwrap();
                    self.push_triple(s, p, o);
                    match self.txt.as_bytes().first() {
                        None | Some(b']') | Some(b'}') => (),
                        _ => self.expect(|c| c == '.')?,
                    }
                    return Ok(());
                }
            }
        }
    }

    fn parse_var_label(&mut self, prefix: &str) -> ParseResult<VarLabel> {
        if self.try_consume_str(prefix) {
            let label = self.parse_suffix();
            LocalId::new(label)
                .map_err(|e| self.err(e))
                .map(VarLabel::from)
        } else {
            match self.txt.as_bytes() {
                [b'<', ..] => self.parse_iri(),
                _ => self.parse_pname(),
            }
            .map(VarLabel::from)
        }
    }

    fn parse_iri(&mut self) -> ParseResult<Iri> {
        let iriref = self.parse_iriref()?;
        self.resolve(iriref).map_err(|e| self.err(e))
    }

    fn parse_iriref(&mut self) -> ParseResult<&'a str> {
        self.expect(|c| c == '<')?;
        let buf = self.txt;
        let c = self.consume_while(|c| c != '>' && !c.is_ascii_whitespace());
        self.expect(|c| c == '>')?;
        Ok(&buf[..c])
    }

    fn parse_pname(&mut self) -> ParseResult<Iri> {
        let pr = self.parse_prefix()?;
        let ns = self
            .config
            .prefixes
            .iter()
            .rev()
            .find_map(|(p, ns)| (p[..] == pr[..]).then(|| ns))
            .ok_or_else(|| self.err(format!("Unknown prefix {:?}", &pr[..])))?;
        let mut iri = ns.to_string();
        let sf = self.parse_suffix();
        iri.push_str(sf);
        Iri::new(iri).map_err(|e| self.err(e))
    }

    fn parse_prefix(&mut self) -> ParseResult<Prefix<'a>> {
        let buf = self.txt;
        let c = self.consume_while(|c| c != ':' && !c.is_ascii_whitespace());
        self.expect(|c| c == ':')?;
        Prefix::new(&buf[..c]).map_err(|e| self.err(e))
    }

    fn parse_suffix(&mut self) -> &'a str {
        let buf = self.txt;
        let mut n = 0;
        loop {
            match self.txt.as_bytes() {
                [] => break,
                [b'.'] => break,
                [c, ..] if !suffix_char(*c) => break,
                [b'.', c, ..] if !suffix_char(*c) => break,
                _ => {
                    n += 1;
                    self.txt = &self.txt[1..];
                }
            }
        }
        self.column += n;
        &buf[..n]
    }

    fn expect<F>(&mut self, mut predicate: F) -> ParseResult
    where
        F: FnMut(char) -> bool,
    {
        if self.try_consume(&mut predicate) {
            Ok(())
        } else {
            Err(self.err(match self.txt.chars().next() {
                None => "Unexpected EOF".into(),
                Some(c) => format!("Unexpected char {:?}", c),
            }))
        }
    }

    fn consume_while<F>(&mut self, mut predicate: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let mut count = 0;
        while self.try_consume(&mut predicate) {
            count += 1;
        }
        count
    }

    #[inline]
    fn try_consume_str(&mut self, prefix: &str) -> bool {
        debug_assert!(!prefix.contains('\n'));
        let n = prefix.len();
        if self.txt.starts_with(prefix) {
            self.txt = &self.txt[n..];
            self.column += n;
            true
        } else {
            false
        }
    }

    #[inline]
    fn try_consume_str_ci(&mut self, prefix: &str) -> bool {
        debug_assert!(!prefix.contains('\n'));
        let n = prefix.len();
        if self.txt.get(..n).unwrap_or("").eq_ignore_ascii_case(prefix) {
            self.txt = &self.txt[n..];
            self.column += n;
            true
        } else {
            false
        }
    }

    #[inline]
    fn try_consume<F>(&mut self, predicate: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        match self.txt.chars().next() {
            Some(c) if predicate(c) => {
                self.txt = &self.txt[c.len_utf8()..];
                if c == '\n' {
                    self.line += 1;
                    self.column = 1;
                    if self.txt.starts_with('\r') {
                        self.txt = &self.txt[1..];
                    }
                } else {
                    self.column += 1;
                }
                true
            }
            _ => false,
        }
    }

    #[inline]
    fn forward(&mut self, txt: &'static str) {
        debug_assert!(!txt.contains('\n'));
        debug_assert!(self.txt.starts_with(txt));
        self.txt = &self.txt[txt.len()..];
        self.column += txt.len();
    }

    fn err<T: ToString>(&self, message: T) -> ParseError {
        ParseError {
            message: message.to_string(),
            line: self.line,
            column: self.column,
        }
    }

    fn set_base(&mut self, iri: &'a str) -> Result<(), InvalidIri> {
        let parsed = IriParsed::new(iri)?;
        self.parsed_base = self.parsed_base.join(&parsed);
        Ok(())
    }

    fn resolve(&self, iri: &'a str) -> Result<Iri, InvalidIri> {
        self.parsed_base
            .resolve(iri)
            .map(|res| Iri::new_unchecked(res.to_string()))
    }

    fn push_existential(&mut self, label: VarLabel) -> usize {
        // TODO detect if the same label is used in the same scope?
        let id = self.next_var;
        self.next_var += 1;
        self.drafts
            .last_mut()
            .unwrap()
            .for_some
            .push((id, label.clone()));
        self.var_labels.insert(label);
        id
    }

    fn push_universal(&mut self, label: VarLabel) -> usize {
        // TODO detect if the same label is used in the same scope?
        let id = self.next_var;
        self.next_var += 1;
        self.drafts
            .last_mut()
            .unwrap()
            .for_all
            .push((id, label.clone()));
        self.var_labels.insert(label);
        id
    }

    fn push_top_universal(&mut self, label: VarLabel) -> usize {
        // TODO detect if the same label is used in the same scope?
        let id = self.next_var;
        self.next_var += 1;
        self.drafts
            .first_mut()
            .unwrap()
            .for_all
            .push((id, label.clone()));
        self.var_labels.insert(label);
        id
    }

    /// Return the appropriate Term for an IRI;
    /// could be an IRI or a Variable (if that IRI is used with a quantifier)
    fn iri_to_term(&self, iri: Iri) -> Term {
        let label1 = iri.into();
        if self.var_labels.contains(&label1) {
            // this IRI is quantified somewhere;
            // let's check that it is still in scope
            // (could be a previously closed subformula)
            for draft in self.drafts.iter().rev() {
                for (id, label2) in &draft.for_all {
                    if &label1 == label2 {
                        return Term::Variable(*id);
                    }
                }
                for (id, label2) in &draft.for_some {
                    if &label1 == label2 {
                        return Term::Variable(*id);
                    }
                }
            }
        }
        match label1 {
            VarLabel::Iri(iri) => iri.into(),
            _ => unreachable!(),
        }
    }

    /// Return the appropriate Term for a quick var;
    /// either it is already quantified in the current scope,
    /// or a top-level quantifier is added
    fn quickvar_to_term(&mut self, name: LocalId) -> Term {
        let label1 = name.into();
        if self.var_labels.contains(&label1) {
            // this quick var is quantified somewhere;
            // let's check that it is still in scope
            // (could be a previously closed subformula)
            for draft in self.drafts.iter().rev() {
                for (id, label2) in &draft.for_all {
                    if &label1 == label2 {
                        return Term::Variable(*id);
                    }
                }
            }
        }
        let id = self.push_top_universal(label1);
        Term::Variable(id)
    }

    /// Return the appropriate Term for a blank node;
    /// either it is already quantified in the current scope,
    /// or a new local quantifier is added
    fn bnode_to_term(&mut self, name: LocalId) -> Term {
        let label1 = name.into();
        if self.var_labels.contains(&label1) {
            // this blank node is quantified somewhere;
            // let's check that it is still in scope
            // (could be a previously closed subformula)
            for draft in self.drafts.iter().rev() {
                for (id, label2) in &draft.for_some {
                    if &label1 == label2 {
                        return Term::Variable(*id);
                    }
                }
            }
        }
        let id = self.push_existential(label1);
        Term::Variable(id)
    }

    fn push_triple(&mut self, s: Term, p: Term, o: Term) {
        self.drafts.last_mut().unwrap().triples.push([s, p, o])
    }
}

// NB: this not an exact
fn suffix_char(c: u8) -> bool {
    !(c.is_ascii_whitespace() || b"{}()[]<>,;-+\"".contains(&c))
}

// FormulaDraft

#[derive(Clone, Debug, Default)]
pub struct FormulaDraft {
    for_some: Vec<(usize, VarLabel)>,
    for_all: Vec<(usize, VarLabel)>,
    triples: Vec<[Term; 3]>,
}

impl From<FormulaDraft> for Formula {
    fn from(other: FormulaDraft) -> Formula {
        Formula::new(other.for_some, other.for_all, other.triples)
    }
}

// ParseError

#[derive(Clone, Debug)]
pub struct ParseError {
    message: String,
    line: usize,
    column: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.line, self.column, self.message)
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T = ()> = Result<T, ParseError>;

//
// TESTS --------------------------------------------------------------------
//

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    type TestResult = Result<(), Box<dyn std::error::Error>>;

    #[test]
    fn parse_sample() -> TestResult {
        let f = parse(
            "
              # this is a comment
            BASE <http://my.example/a/>
            PREFIX a: <>
            @prefix b: <../b/>.

            @forSome :ppl1, :ppl2.

            a:alice a b:Person, b:Woman;
              # an intermediate comment
              b:likes (a:bob.: :ppl1 :ppl2 true).
            # a:this_triple a:will_be a:ignore.
            
            {?x a :Person} => {?x :mother _:mother}.
        ",
            Default::default(),
        )?;
        let triples = f.triples();
        assert_eq!(triples.len(), 4);
        assert_eq!(triples[0][0], Iri::new("http://my.example/a/alice")?.into());
        assert_eq!(&triples[0][1], &rdf::type_.into());
        assert_eq!(
            &triples[0][2],
            &Iri::new("http://my.example/b/Person")?.into()
        );
        assert_eq!(triples[1][0], Iri::new("http://my.example/a/alice")?.into());
        assert_eq!(&triples[1][1], &rdf::type_.into());
        assert_eq!(
            &triples[1][2],
            &Iri::new("http://my.example/b/Woman")?.into()
        );
        assert_eq!(
            &triples[2][0],
            &Iri::new("http://my.example/a/alice")?.into()
        );
        assert_eq!(
            &triples[2][1],
            &Iri::new("http://my.example/b/likes")?.into()
        );
        assert_eq!(
            &triples[2][2],
            &vec![
                Iri::new("http://my.example/a/bob.:")?.into(),
                Term::Variable(0),
                Term::Variable(1),
                Literal::Boolean(true).into(),
                //Literal::Boolean(false).into(),
            ]
            .into()
        );
        assert!(matches!(&triples[3][0], Term::Formula(f) if f.triples().len() == 1));
        assert_eq!(&triples[3][1], &log::implies.into());
        assert!(matches!(&triples[3][2], Term::Formula(f) if f.triples().len() == 1));
        Ok(())
    }

    #[test_case(":a :b { :c :d :e"; "missing closing curly bracket")]
    #[test_case(":a :b :c }"; "spurious closing curly bracket")]
    fn invalid_syntax(src: &str) {
        assert!(parse(src, Default::default()).is_err());
    }
}
