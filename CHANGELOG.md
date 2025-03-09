# Change Log: mir-rs

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/)
and this project adheres to [Semantic Versioning](https://semver.org/).

## v0.3.0

### Added

- `struct ItemType`, returned by `ItemRef::type_`.

### Changed

- `Label` created now has full-module lifetime, respecting upstream docs.

### Removed

- `FuncInstBuilder` type is removed from public API.

  `FuncBuilder::ins` now returns an opaque `impl InsnBuilder` that is expected
  to be consumed directly.

### Others

- Fixed typos.

- Panic messages from C errors are improved.

- Rename C trampoline libname to be less generic, to avoid name collisions.

- `MirContext` states are tweaked to reduce its size.

## v0.2.0

Initial release.
