
# [WIP] `otfed`: OpenType Font Format Encoder & Decoder

This library is intended to be a reformulation and extension of [`otfm`](https://github.com/dbuenzli/otfm).

See also:

* [an extended version of `otfm` for SATySFi](https://github.com/gfngfn/otfm)
* [SATySFi](https://github.com/gfngfn/SATySFi)

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
  <tr><td>glyf</td><td>-</td><td>-</td><td>v</td><td>-</td></tr>

  <tr><td rowspan="3">CFF</td>
      <td>CFF␣</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>CFF2</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>VORG</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>

  <tr><td rowspan="5">Advanced</td>
      <td>BASE</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>GPOS</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>GSUB</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>MATH</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
  <tr><td>kern</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
</table>
