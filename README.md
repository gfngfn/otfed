
# [WIP] `otfed`: OpenType Font Format Encoder & Decoder

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


## Usage of an example CLI `otfedcli`

```console
$ dune exec otfedcli <path/to/font-file> <commands>

<commands> ::= [<command>]*

<command> ::=
  | tables                             # Prints all the tags of tables contained in the font.
  | cmap                               # Prints all the Unicode-aware `cmap` subtables.
  | head                               # Prints the contents of `head` table.
  | hhea                               # Prints the contents of `hhea` table.
  | maxp                               # Prints the contents of `maxp` table.
  | glyf <glyph-id> <output-svg-file>  # Outputs the glyph of ID <glyph-id> that has TrueType outlines.
  | cff <glyph-id> <output-svg-file>   # Outputs the glyph of ID <glyph-id> that has CFF outlines.
  | gsub <script> <langsys> <feature>  # Prints the contents of `GSUB` subtables.
  | gpos <script> <langsys> <feature>  # Prints the contents of `GPOS` subtables.
```

### Example usage

Prints all of the Unicode-aware `cmap` subtables in IPAex Mincho:

```console
$ dune exec otfedcli input/ipaexm.ttf cmap
```

Outputs the glyph of ID 1000 in IPAex Mincho as an SVG file:

```console
$ dune exec otfedcli input/ipaexm.ttf glyf 1000 output/ipaexm1000.svg
```

Outputs the glyph of ID 50 in Computer Modern Typewriter Italic as an SVG file:

```console
$ dune exec otfedcli input/cmunit.otf cff 50 output/cmunit1000.svg
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
      <td>cmap</td><td>-</td><td>-</td><td>v (Format 4, 12, and 13 only)</td><td>-</td></tr>
  <tr><td>head</td><td>-</td><td>-</td><td>v</td><td>-</td></tr>
  <tr><td>hhea</td><td>-</td><td>-</td><td>v</td><td>-</td></tr>
  <tr><td>hmtx</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>maxp</td><td>-</td><td>-</td><td>v</td><td>-</td></tr>
  <tr><td>name</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>OS/2</td><td>-</td><td>-</td><td>v</td><td>-</td></tr>
  <tr><td>post</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="2">TTF</td>
      <td>loca</td><td>-</td><td>-</td><td>v</td><td>-</td></tr>
  <tr><td>glyf</td><td>-</td><td>-</td><td>v</td><td>v (simple glyphs only)</td></tr>

  <tr><td rowspan="3">CFF</td>
      <td>CFF␣</td><td>-</td><td>-</td><td>v</td><td>v</td></tr>
  <tr><td>CFF2</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>VORG</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="5">Advanced</td>
      <td>BASE</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>GPOS</td><td>-</td><td>-</td><td>v (LookupType 1, 2, 4, 5, 6, and 9 only)</td><td>-</td></tr>
  <tr><td>GSUB</td><td>-</td><td>-</td><td>v (LookupType 1, 2, and 4 only)</td><td>-</td></tr>
  <tr><td>MATH</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>kern</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
</table>
