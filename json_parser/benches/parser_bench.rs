use criterion::{black_box, criterion_group, criterion_main, Criterion};
use json_parser::{parse_streaming, JsonValue}; // Your library
use serde_json::Value; // The standard library's JSON value
use std::collections::HashMap;

// A sample "medium" JSON file content
const MEDIUM_JSON: &str = r#"
{
    "name": "Babbage",
    "age": 30,
    "admin": true,
    "friends": ["Ada", "Charles", "Grace"],
    "tasks": [
        { "id": 1, "title": "Parse JSON", "done": false },
        { "id": 2, "title": "Write docs", "done": true }
    ],
    "nested": {"key": [null, 1, 1.23e4]}
}
"#;

fn bench_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("JSON Parsing");

    // Benchmark your parser
    group.bench_function("My Streaming Parser", |b| {
        b.iter(|| {
            // We must consume the iterator to do the work
            let parser = parse_streaming(black_box(MEDIUM_JSON)).unwrap();
            let _ = parser.count(); // .count() consumes the iterator
        })
    });

    // Benchmark serde_json
    group.bench_function("serde_json::from_str", |b| {
        b.iter(|| {
            let _: Value = serde_json::from_str(black_box(MEDIUM_JSON)).unwrap();
        })
    });

    group.finish();
}

fn bench_stringifying(c: &mut Criterion) {
    // Create your native JsonValue
    let mut my_val_map = HashMap::new();
    my_val_map.insert("key".to_string(), JsonValue::String("value".to_string()));
    my_val_map.insert(
        "items".to_string(),
        JsonValue::Array(vec![JsonValue::Number(1.0), JsonValue::Null]),
    );
    let my_value = JsonValue::Object(my_val_map);

    // Create the equivalent serde_json::Value
    let mut serde_val_map = serde_json::Map::new();
    serde_val_map.insert(
        "key".to_string(),
        serde_json::Value::String("value".to_string()),
    );
    serde_val_map.insert(
        "items".to_string(),
        serde_json::Value::Array(vec![
            serde_json::Value::Number(serde_json::Number::from_f64(1.0).unwrap()),
            serde_json::Value::Null,
        ]),
    );
    let serde_value = serde_json::Value::Object(serde_val_map);

    let mut group = c.benchmark_group("JSON Stringify");

    // Benchmark your stringify
    group.bench_function("My Stringify", |b| {
        b.iter(|| {
            let _ = my_value.stringify();
        })
    });

    // Benchmark serde_json::to_string
    group.bench_function("serde_json::to_string", |b| {
        b.iter(|| {
            let _ = serde_json::to_string(&serde_value).unwrap();
        })
    });

    group.finish();
}

criterion_group!(benches, bench_parsing, bench_stringifying);
criterion_main!(benches);
