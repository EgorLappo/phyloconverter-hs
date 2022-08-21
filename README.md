# Phylogenetic format converter

As an educational project, I am implementing a file format converter for phylogenetic tree data using `Haskell` and `Parsec`.

*THIS IS AN EDUCATIONAL PROJECT, FUNCTIONALITY IS LIMITED AND POSSIBLY FLAWED, CAUTION IS REQUIRED*

The formats supported so far are

* __`Newick`__, which was implemented using the grammar specification from [Wikipedia](https://en.wikipedia.org/wiki/Newick_format#cite_note-2). It currently _does not_ support comments or any extended features. The labels of taxa could be abitrary _single-quoted_ strings, or unquoted strings of alphanumeric characters together with `_`, `-`, or `.` (no whitespace).

* __`Nexus`__, which was implemented using the specification from [http://wiki.christophchamp.com/index.php?title=NEXUS_file_format](http://wiki.christophchamp.com/index.php?title=NEXUS_file_format). 

* __TODO: `PhyloXML`__, documents like [this one](http://www.phyloxml.org/examples_syntax/phyloxml_syntax_example_1.html).

## Usage

To use this program, use `./phyloconverter [input file] [output file]`.
