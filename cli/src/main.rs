use clap::{App, Arg};
use namur::*;
use std::io::{stdin, BufRead, BufReader};

#[allow(clippy::redundant_clone)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    /*
    use notation3::ns::{log, rdf};
    use Term::Variable;
    let alice = Term::from(Iri::new("http://example.org/alice")?);
    let person = Term::from(Iri::new("http://example.org/Person")?);
    let name = Term::from(Iri::new("http://example.org/knows")?);
    let age = Term::from(Iri::new("http://example.org/age")?);
    let knows = Term::from(Iri::new("http://example.org/knows")?);
    let bob = Term::from(Iri::new("http://example.org/bob")?);
    let charlie = Term::from(Iri::new("http://example.org/charlie")?);
    let likes = Term::from(Iri::new("http://example.org/likes")?);

    let lit_alice = Literal::from("Alice");
    let lit_42 = Literal::from(42);

    let f1 = Formula::from(vec![[Variable(1), rdf::type_.into(), person.clone()]]);
    let f2 = Formula::from(vec![[alice.clone(), likes, Variable(1)]]);

    let doc = Formula::new(
        vec![],
        vec![(1, LocalId::new("x")?.into())],
        vec![
            [alice.clone(), rdf::type_.into(), person],
            [alice.clone(), knows, vec![bob, charlie].into()],
            [alice.clone(), name, lit_alice.into()],
            [alice.clone(), age, lit_42.into()],
            [f1.into(), log::implies.into(), f2.into()],
        ],
    );

    serialize(&doc, Default::default())?;
    */

    let matches = App::new("n3r")
        .version("0.1.0")
        .author("Pierre-Antoine Champin <http://champin.net/>")
        .about("A Notation3 Reasoner")
        .arg(
            Arg::new("think")
                .short('t')
                .long("think")
                .multiple_occurrences(true)
                .help("Computes one step of forward-chaining reasoning, and output results"),
        )
        .get_matches();

    println!("\n\n\n### Enter some N3...");
    let mut txt = String::new();
    for line in BufReader::new(stdin()).lines() {
        txt.push_str(&line?);
    }
    let f = parse(&txt, Default::default())?;
    println!("\n\n\n### Here's your formula:");
    serialize(&f, Default::default())?;

    match matches.occurrences_of("think") {
        0 => (),
        1 => {
            println!("\n\n### After thinking step #{}", 1);
            for result in fc_reason(&f) {
                serialize(&result, Default::default())?;
                println!("\n### ----")
            }
        }
        _ => todo!("multiple steps of thinking"),
    }
    Ok(())
}
