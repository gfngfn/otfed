
# `otfed`: OpenType Font Format Encoder & Decoder

This library is intended to be a reformulation and extension of [`otfm`](https://github.com/dbuenzli/otfm).

See also:

* [an extended version of `otfm` for SATySFi](https://github.com/gfngfn/otfm)
* [SATySFi](https://github.com/gfngfn/SATySFi)


## Note

Some unit tests use data extracted from the following fonts:

* [IPAex明朝 (IPAex Mincho)](https://moji.or.jp/ipafont/), `ipaexm.ttf`
  - See the license [here](https://moji.or.jp/ipafont/license/)
* [Latin Modern](http://www.gust.org.pl/projects/e-foundry/latin-modern), `lmroman10-regular.otf`
  - See the license [here](http://www.gust.org.pl/projects/e-foundry/latin-modern#section-2)


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
$ dune exec otfedcli <path/to/font-file> <commands>

<commands> ::= [<command>]*

<command> ::=
  | tables                             # Prints all the tags of tables contained in the font.
  | cmap                               # Prints all the Unicode-aware `cmap` subtables.
  | cmap_word "<arbitrary-utf8-text>"  # Consults `cmap` subtables for each character in the given text.
  | head                               # Prints the contents of `head` table.
  | hhea                               # Prints the contents of `hhea` table.
  | maxp                               # Prints the contents of `maxp` table.
  | hmtx <glyph-id>                    # Consults the `hmtx` table by the glyph of ID <glyph-id>.
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


## Status

<table>
  <tr>
    <th rowspan="2" colspan="2">Tables</th>
    <th colspan="2">Encoding operations</th>
    <th colspan="3">Decoding operations</th>
  </tr>
  <tr>
    <th>Supported</th><th>Tested</th><th>Supported</th><th>Tested</th>
  </tr>
  <tr><td rowspan="8">Required</td>
      <td>cmap</td><td>v (Format 12 only)</td><td>o</td><td>v (Format 4, 12, and 13 only)</td><td>o</td></tr>
  <tr><td>head</td><td>v</td><td>v</td><td>v</td><td>v</td></tr>
  <tr><td>hhea</td><td>v</td><td>v</td><td>v</td><td>v</td></tr>
  <tr><td>hmtx</td><td>v</td><td>o</td><td>v</td><td>o</td></tr>
  <tr><td>maxp</td><td>v</td><td>o</td><td>v</td><td>o</td></tr>
  <tr><td>name</td><td>v</td><td>o</td><td>v</td><td>o</td></tr>
  <tr><td>OS/2</td><td>v</td><td>o</td><td>v</td><td>o</td></tr>
  <tr><td>post</td><td>v (version 3 only)</td><td>o</td><td>v</td><td>o</td></tr>

  <tr><td rowspan="6">TTF</td>
      <td>cvt␣</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>fpgm</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>glyf</td><td>v</td><td>v (simple glyphs only)</td><td>v</td><td>v (simple glyphs only)</td></tr>
  <tr><td>loca</td><td>v</td><td>o</td><td>v</td><td>o</td></tr>
  <tr><td>prep</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>gasp</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="3">CFF</td>
      <td>CFF␣</td><td>v</td><td>o</td><td>v</td><td>v</td></tr>
  <tr><td>CFF2</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>VORG</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="1">SVG</td>
      <td>SVG␣</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="4">Optional</td>
      <td>DSIG</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>kern</td><td>-</td><td>-</td><td>v (Format 0 only)</td><td>o</td></tr>
  <tr><td>vhea</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>vmtx</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="7">Advanced</td>
      <td>BASE</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>GDEF</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>GPOS</td><td>-</td><td>-</td><td>v (LookupType 1, 2, 4, 5, 6, and 9 only)</td><td>o</td></tr>
  <tr><td>GSUB</td><td>-</td><td>-</td><td>v (LookupType 1, 2, and 4 only)</td><td>o</td></tr>
  <tr><td>JSTF</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>MATH</td><td>-</td><td>-</td><td>v</td><td>o</td></tr>
</table>

* v: done
* o: no automated test has been given, but seems working correctly for many inputs.


## How to develop

Assumes that [Dune](https://dune.build/) (≥2.7) is installed.

### How to build

```console
$ dune build
```

### How to run tests

```console
$ dune test
```
