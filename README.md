# ebnf-extensions

Some type synonyms and extensions to BNF, with generators and parsers:

- `Opt`: synonym for `Data.Maybe.Maybe` (an optional element)
- `Rep0`: synonym for `[]` (a possibly empty list of elements)
- `Rep1`: synonym for `Data.List.NonEmpty.NonEmpty`
    (a non-empty list of elements)
- `Repsep0`: datatype for a possibly empty list of elements with separators
- `Repsep1`: datatype for a non-empty list of elements with separators

