# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).


## [Unreleased]

## [0.3.0] - 2023-10-01
### Changed
- Fix how to handle `OS/2`'s `usWeightClass` ([PR\#52](https://github.com/gfngfn/otfed/pull/52); breaking change).

### Fixed
- Fix how to decode Format 0 subtables of `kern` tables ([PR\#53](https://github.com/gfngfn/otfed/pull/53)).
- Fix `Value.Cmap.Mapping.add_{incremental,constant}_range` about handling the maximum codepoint ([PR\#55](https://github.com/gfngfn/otfed/pull/55)).
- Fix how to subset fonts and how to encode fractions ([PR\#57](https://github.com/gfngfn/otfed/pull/57)).

## [0.2.0] - 2023-06-27
### Changed
- Rename `Hhea.t`'s `xmax_extent` to `x_max_extent` ([PR\#45](https://github.com/gfngfn/otfed/pull/45); breaking change).
- Change the interface of `Otfed.Decode.Cmap.get_subtables` ([PR\#47](https://github.com/gfngfn/otfed/pull/47); breaking change).

### Added
- Support `vhea` tables ([PR\#45](https://github.com/gfngfn/otfed/pull/45)).
- Support `vmtx` tables ([PR\#46](https://github.com/gfngfn/otfed/pull/46)).
- Support decoding of `GPOS` Lookup Type 3, i.e., subtables for cursive attachment ([PR\#49](https://github.com/gfngfn/otfed/pull/49)).

## [0.1.0] - 2023-05-26
### Changed
- Make `Otfed.Decode.Ttf.loca` distinguish "having an empty glyph" from "undefined" (breaking change).

### Fixed
- Fix `Otfed.Subset.make` about subsetting fonts that contain empty glyphs.

## 0.0.1 - 2022-10-22
### Added
- Initial version of Otfed


  [Unreleased]: https://github.com/gfngfn/otfed/compare/0.3.0...HEAD
  [0.3.0]: https://github.com/gfngfn/otfed/compare/0.2.0...0.3.0
  [0.2.0]: https://github.com/gfngfn/otfed/compare/0.1.0...0.2.0
  [0.1.0]: https://github.com/gfngfn/otfed/compare/0.0.1...0.1.0
