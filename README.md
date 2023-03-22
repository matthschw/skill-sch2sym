# skill-sch2sym
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Convert a Cadence Virtuoso Schematic in a Symbol

## Installation

The functionality can be used by loading the SKILL file *EDsch2sym.il* in 
Cadence Virtuoso:

``` scheme
(load "EDsch2sym.il")
```
A more convenient way might be to add this load command in the `.cdsinit`.

## Usage

The Skill command `EDsch2sym` is used to convert the schematic in the 
symbol.
Have a look in *EDsch2sym.il*  for the parameters of the function.

## Example

By executing the command
``` scheme
(EDsch2sym
  (dbOpenCellViewByType "experiments" "cm" "schematic" "schematic" "r")
  (dbOpenCellViewByType "experiments" "cm" "symbol" "schematicSymbol" "w")
  ?rightMargin 0.25
  ?bottomMargin 0.25
  ?scale 1.0
);EDsch2sym
```
the schematic

<img src="./figs/sch.png" width="640">

in converted in the below shown symbol

<img src="./figs/sch.png" width="sym">

## License

Copyright (c) 2023 [Electronics & Drives](https://www.electronics-and-drives.de/) 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.