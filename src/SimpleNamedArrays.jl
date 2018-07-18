"""
A `NArray` is the marriage of a standard julia `Array` and a list of
names for the columns of a `Matrix`
( or the (1:N-1...) `Array{N-1}`s of an `Array{N}` ).
The 'column' names can be anything, but will likely be `String`,
`Symbol`, or `Column` (which is itself a special `AbstractString`)
"""
module SimpleNamedArrays

# A NArray should carry size and type information like an Array
# but also the column type.
include("Columns.jl")
include("NArrays.jl")
end # module
