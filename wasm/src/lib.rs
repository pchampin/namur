use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, hello-wasm!");
}

#[wasm_bindgen]
pub struct N3Formula(namur::Formula);

fn serialize(f: &namur::Formula) -> String {
    let mut buf = Vec::<u8>::new();

    let config = namur::SerializeConfig::new(&mut buf)
        .with_prefix(
            sophia_api::prefix::PrefixBox::new("".into()).unwrap(),
            namur::Iri::new("http://example.org/").unwrap(),
        )
        .with_strict(true);
    namur::serialize(f, config).unwrap();
    String::from_utf8(buf).unwrap()
}

#[wasm_bindgen]
impl N3Formula {
    #[wasm_bindgen(constructor)]
    pub fn new(txt: &str) -> Result<N3Formula, JsValue> {
        #[cfg(feature = "console_error_panic_hook")]
        console_error_panic_hook::set_once();

        namur::parse(txt, Default::default())
            .map(Self)
            .map_err(|e| e.to_string().into())
    }

    #[wasm_bindgen]
    pub fn serialize(&self) -> String {
        serialize(&self.0)
    }

    #[wasm_bindgen]
    pub fn think(&self) -> String {
        namur::fc_reason(&self.0)
            .iter()
            .map(serialize)
            .fold("".to_string(), |mut acc, i| {
                acc.push_str(&i);
                acc.push_str("\n\n");
                acc
            })
    }
}
