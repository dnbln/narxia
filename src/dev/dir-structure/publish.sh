#!/usr/bin/env bash
(cd dir-structure-resolve-core && cargo publish)
(cd dir-structure-macros && cargo publish)
(cd dir-structure && cargo publish)
(cd dir-structure-tools && cargo publish)
(cd dir-structure-git-vfs && cargo publish)
(cd dir-structure-include-dir-vfs && cargo publish)
