# Changelog

## Version 0.3.0

- Added support for custom sections in --help.

- Added support for synonyms: arguments can now have multiple equivalent names,
  not just at most one long and one short.

- Added `flag_enum`, a generalization of `flag` for enums
  with more than 2 possible values.

## Version 0.2.0

- Fixed `Not_found` error when `TERM` is not set.
  This should in particular help on Windows.

- Added support for negative integer arguments.

- Fixed some typos.

## Version 0.1.0

First release of Clap.
