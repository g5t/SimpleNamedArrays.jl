
# SimpleNamedArrays
___
## NArray
Implements the type `NArray` which is a standard `Array` that includes one dimension that can be indexed by name.
Another package, NamedArrays.jl, appears to do the same thing but with arbitrary named dimensions, as this leads
to a somewhat complex named accessing scheme, this package is SimpleNamedArrays.jl

The signature for a `NArray` contains the dimensionality and element type of the underlying julia `Array` plus
the name type, e.g., `NArray{Float64,2,Symbol}` would indicate a julia `Matrix` of `Float64` elements with
`Symbol` names.
- Any element type which is allowed by a julia `Array` is allowed by a `NArray`
- Any dimensionality of `NArray` is allowed
- The allowed name types are anything for which `isequal` has been implemented. *This does mean that you can use `Integer` names, but doing so is probably a bad idea.*

At some level a `NArray{T,N,C}` is similar to a `Dict{Array{T,N-1},C}` but it has the advantage(?) of
being densly packed in memory, allowing for integer and boolean indexing of the named dimension -- this also
ensures that the sizes of the "sub-arrays" (`Array{T,N-1}`) are all the same, which could otherwise be enforced
by using, e.g., StaticArrays in the dictionary.

## Column
A special type, intended to be the names of a special `NArray`, is implemented.
The `Column` type is esentially a highly specialized `AbstractString` and is, at a minimum, a name string.
The real utility of the `Column` type comes from its ability to contain a unit and another normalization `Column`, e.g.,
`"detector [counts/monitor counts]"` is the string representation of
`Column("detector",unit="counts",norm=Nullable(Column("monitor",unit="counts")))`

**TODO:** implement something like `c"point"`, `c"temperature/K"`, and `c"detector [counts/monitor counts]"` to create `Column` objects.

One can also create objects of the `Column` type using the exported macro `@c_str`
which takes a string input of any of the display forms of a `Column` and parses
it into a new object. This means that it is also possible to use the built-in
julia functionality of non-standard string litterals to create `Column` objects
via, e.g., `c"detector [counts/3×10⁵ monitor counts]"`.
