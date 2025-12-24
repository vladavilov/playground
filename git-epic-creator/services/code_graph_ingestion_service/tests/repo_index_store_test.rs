use code_graph_ingestion_service::persistence::repo_index_store::{canonical_json_bytes, sha256_hex};
use serde_json::json;

#[test]
fn canonical_json_is_stable_across_key_order() {
    let a = json!({"b": 1, "a": {"y": 2, "x": 3}, "c": [ {"k": 1, "j": 2} ]});
    let b = json!({"c": [ {"j": 2, "k": 1} ], "a": {"x": 3, "y": 2}, "b": 1});

    let ca = canonical_json_bytes(&a);
    let cb = canonical_json_bytes(&b);
    assert_eq!(ca, cb);
    assert_eq!(sha256_hex(&ca), sha256_hex(&cb));
}


