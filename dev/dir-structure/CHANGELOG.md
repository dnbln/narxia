# `0.1.5`

Released: 2025-04-16

## `Filter`

Allows for filtering of `DirChildren` entries.

## Support for toml, yaml, ron files.

Similarly to `json`, you can now also use `toml`, `yaml`, and `ron` files, by enabling the respective feature flags.

## Lots of documentation and examples

Added a lot of documentation and examples to the code.

# `0.1.4`

Released: 2024-04-15

## `Versioned`

New wrapper type: [`Versioned`](https://docs.rs/dir-structure/latest/dir_structure/struct.Versioned.html).

It will only write the value to disk if it has changed in Rust
code since it has been read. Refer to documentation for how to use.
