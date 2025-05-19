
[group("config")]
[doc("Set up the env.")]
bootstrap:
    # Used for generating profile information; see `flame` below.
    cargo install flamegraph

    # See https://github.com/mstange/samply?tab=readme-ov-file
    cargo install --locked samply
    cargo install cargo-samply
    brew install defaultbrowser

[group("debug")]
[doc('Generate a flamegraph; will create a file in ./flamegraph.svg')]
flame PATH_TO_SCAN:
    sudo CARGO_PROFILE_RELEASE_DEBUG=true \
    cargo flamegraph --bin tp -- \
    scan -p  PATH_TO_SCAN

# WARNING: the `--cfg samply_kluges` is important; see Config.toml and  tp.rs.

[group("debug")]
[doc('Run Samply')]
profile-scan PATH_TO_SCAN:
    sudo  \
    CARGO_PROFILE_RELEASE_DEBUG=true \
    RUSTFLAGS="-C link-args=-Wl,-rpath,/opt/homebrew/lib --cfg samply_kludges" \
    cargo samply --bin tp -- \
    scan -p  PATH_TO_SCAN

[group("debug")]
[doc('Run Samply')]
profile-cluster PATH_TO_SCAN:
    sudo  \
    CARGO_PROFILE_RELEASE_DEBUG=true \
    RUSTFLAGS="-C link-args=-Wl,-rpath,/opt/homebrew/lib --cfg samply_kludges" \
    cargo samply --bin tp -- \
    cluster -p  PATH_TO_SCAN

install:
    RUSTFLAGS="-C link-args=-Wl,-rpath,/opt/homebrew/lib" \
        cargo install --no-default-features --profile release --path .
