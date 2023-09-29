
# `otfed`: OpenType Font Format Encoder & Decoder

## Table of Contents

- [How to install](#how-to-install)
- [Usage of an example CLI `otfedcli`](#usage-of-an-example-cli-otfedcli)
- [Development status](#development-status)
- [How to develop this library](#how-to-develop-this-library)
- [Remarks](#remarks)


## How to install

Under the condition that [`opam`](https://opam.ocaml.org/) (a package manager for OCaml, Coq, etc.) is installed, invoke the following commands:

```console
$ cd <your-workspace>
$ git clone https://github.com/gfngfn/otfed
$ cd otfed
$ opam pin add otfed .
```

Probably the last command above will ask you whether to install `otfed`. Then answer `y`.

If not, invoking the following command will do:

```console
$ opam install otfed
```


## Usage of an example CLI `otfedcli`

```console
$ dune exec otfedcli <path/to/font-file> <command> ... <command>

<command> ::=
  | tables                             # Prints all the tags of tables contained in the font.
  | cmap                               # Prints all the Unicode-aware `cmap` subtables.
  | cmap_word "<arbitrary-utf8-text>"  # Consults `cmap` subtables for each character in the given text.
  | head                               # Prints the contents of `head` table.
  | hhea                               # Prints the contents of `hhea` table.
  | vhea                               # Prints the contents of `vhea` table.
  | maxp                               # Prints the contents of `maxp` table.
  | hmtx <glyph-id>                    # Consults the `hmtx` table by the glyph of ID <glyph-id>.
  | vmtx <glyph-id>                    # Consults the `vmtx` table by the glyph of ID <glyph-id>.
  | glyf <glyph-id> <output-svg-file>  # Outputs the glyph of ID <glyph-id> that has TrueType outlines.
  | cff <glyph-id> <output-svg-file>   # Outputs the glyph of ID <glyph-id> that has CFF outlines.
  | cff_lex <glyph-id>                 # Prints the tokenized CharString of the glyph of ID <glyph-id>.
  | cff_top                            # Prints the Top DICT in the `CFF␣` table.
  | charset <glyph-id>                 # Prints the name of the glyph by consulting the charset in `CFF␣`.
  | gsub <script> <langsys> <feature>  # Prints the contents of `GSUB` subtables.
  | gpos <script> <langsys> <feature>  # Prints the contents of `GPOS` subtables.
  | subset <glyph-ids> <output-ttf>    # Makes a subset font by using given glyph IDs.

<glyph-ids> ::= (comma-separated glyph IDs where no space is allowed around commas)
```

### Example usage

Consults `cmap` subtables by Unicode code points:

```console
$ dune exec otfedcli input/ipaexm.ttf cmap_word "田中太郎"
* subtable (platform: 0, encoding: 3)
  - U+7530 --> 2900
  - U+4E2D --> 2746
  - U+592A --> 2614
  - U+90CE --> 3830
* subtable (platform: 3, encoding: 1)
  - U+7530 --> 2900
  - U+4E2D --> 2746
  - U+592A --> 2614
  - U+90CE --> 3830
* subtable (platform: 3, encoding: 10)
  - U+7530 --> 2900
  - U+4E2D --> 2746
  - U+592A --> 2614
  - U+90CE --> 3830
```

Prints all of the Unicode-aware `cmap` subtables in IPAex Mincho (please be careful of large outputs on stdout):

```console
$ dune exec otfedcli input/ipaexm.ttf cmap
(omitted)
```

Outputs the glyph of ID 1000 in IPAex Mincho as an SVG file:

```console
$ dune exec otfedcli input/ipaexm.ttf glyf 1000 output/ipaexm1000.svg
(omitted)
```

Outputs the glyph of ID 50 in Computer Modern Typewriter Italic as an SVG file:

```console
$ dune exec otfedcli input/cmunit.otf cff 50 output/cmunit1000.svg
(omitted)
```

Outputs the subset of Junicode which contains `.notdef`, “Q”, and “f” only:

```console
$ dune exec otfedcli input/Junicode.ttf subset 0,113,302 output/Junicode-subset.ttf
```


## Development status

* Support:
  - V = supported
  - \- = not supported yet
* Test:
  - V = having automated tests
  - \- = not supported
  - o = no automated test has been given, but seems to be working fine for many inputs
  - x = not well-tested

<table>
  <tr><th rowspan="2" colspan="3">Tables</th>                                       <th colspan="2">Encoding operations</th><th colspan="2">Decoding operations</th></tr>
  <tr>                                                                              <th>Supported</th><th>Tested</th><th>Supported</th><th>Tested</th></tr>
  <tr><td rowspan="19">Required</td><td rowspan="9">cmap</td><td>Format 0</td>      <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>Format 2</td>      <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>Format 4</td>      <td>-</td><td>-</td><td>V</td><td>V</td></tr>
  <tr>                                                       <td>Format 6</td>      <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>Format 8</td>      <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>Format 10</td>     <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>Format 12</td>     <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                                                       <td>Format 13</td>     <td>-</td><td>-</td><td>V</td><td>V</td></tr>
  <tr>                                                       <td>Format 14</td>     <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">head</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">hhea</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">hmtx</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">maxp</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">name</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td rowspan="4">OS/2</td><td>ver. 0</td>        <td>V</td><td>o</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>ver. 1</td>        <td>V</td><td>o</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>ver. 2</td>        <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                                                       <td>ver. 3, 4, & 5</td><td>V</td><td>o</td><td>V</td><td>o</td></tr>
  <tr>                              <td colspan="2">post</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>

  <tr><td rowspan="7">TTF</td>      <td colspan="2">cvt␣</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">fpgm</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">glyf</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                              <td rowspan="2">loca</td><td>short</td>         <td>V</td><td>V</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>long</td>          <td>V</td><td>o</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">prep</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">gasp</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="3">CFF</td>      <td colspan="2">CFF␣</td>                       <td>V</td><td>o</td><td>V</td><td>V</td></tr>
  <tr>                              <td colspan="2">CFF2</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">VORG</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td>SVG</td>                  <td colspan="2">SVG␣</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="6">Optional</td> <td colspan="2">DSIG</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td rowspan="2">kern</td><td>Format 0</td>      <td>-</td><td>-</td><td>V</td><td>V</td></tr>
  <tr>                                                       <td>other</td>         <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td rowspan="2">vhea</td><td>ver. 1.0</td>      <td>V</td><td>V</td><td>V</td><td>V</td></tr>
  <tr>                                                       <td>ver. 1.1</td>      <td>V</td><td>x</td><td>V</td><td>x</td></tr>
  <tr>                              <td colspan="2">vmtx</td>                       <td>V</td><td>V</td><td>V</td><td>V</td></tr>

  <tr><td rowspan="21">Advanced</td><td colspan="2">BASE</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">GDEF</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td rowspan="9">GPOS</td><td>LookupType 1</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 2</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 3</td>  <td>-</td><td>-</td><td>V</td><td>x</td></tr>
  <tr>                                                       <td>LookupType 4</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 5</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 6</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 7</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>LookupType 8</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>LookupType 9</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                              <td rowspan="8">GSUB</td><td>LookupType 1</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 2</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 3</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>LookupType 4</td>  <td>-</td><td>-</td><td>V</td><td>o</td></tr>
  <tr>                                                       <td>LookupType 5</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>LookupType 6</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>LookupType 7</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                                                       <td>LookupType 8</td>  <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">JSTF</td>                       <td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr>                              <td colspan="2">MATH</td>                       <td>-</td><td>-</td><td>V</td><td>V</td></tr>
</table>


## How to develop this library

Assumes that [Dune](https://dune.build/) (≥2.7) is installed.

### How to build

```console
$ dune build
```

### How to run tests

```console
$ dune test
```


## Remarks

### Origin

This library has been developed with the intension of reformulating [`otfm`](https://github.com/dbuenzli/otfm).

See also:

* [an extended version of `otfm` for SATySFi](https://github.com/gfngfn/otfm)
* [SATySFi](https://github.com/gfngfn/SATySFi)


### Data used in unit tests

Some unit tests use data extracted from the following fonts:

* [IPAex明朝 (IPAex Mincho)](https://moji.or.jp/ipafont/): `ipaexm.ttf`
  - See the license [here](https://moji.or.jp/ipafont/license/)
* [Latin Modern](http://www.gust.org.pl/projects/e-foundry/latin-modern): `lmroman10-regular.otf` and `lmmono10-regular.otf`
  - See the license [here](http://www.gust.org.pl/projects/e-foundry/latin-modern#section-2)
* [Latin Modern Math](https://www.gust.org.pl/projects/e-foundry/lm-math)
  - See the license [here](https://www.latex-project.org/lppl/)
* [Junicode](https://junicode.sourceforge.io/)
  - See the license [here](https://github.com/psb1558/Junicode-font/blob/master/OFL.txt)
* [DejaVu Sans](https://dejavu-fonts.github.io/): `DejaVuSans-ExtraLight.ttf`
  - See the license [here](https://dejavu-fonts.github.io/License.html)
