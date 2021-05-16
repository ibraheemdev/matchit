#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: (Vec<(String, i32)>, String)| {
    let mut matcher = matchit::Node::new();

    for (key, item) in data.0 {
        if matcher.insert(key, item).is_err() {
            return;
        }
    }

    let _ = matcher.at(&data.1);
});
