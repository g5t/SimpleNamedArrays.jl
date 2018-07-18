export NArray, hasname, addname!, replacename!, nametype, getnames, getarray, array
mutable struct NArray{T,N,C} <: AbstractArray{T,N}
    array::Array{T,N}
    names::Vector{C}
    function NArray{T,N,C}(a::Array{T,N},n::Vector{C}) where {T,N,C}
        @assert size(a,N)==length(n) "$(size(a)) Arrray and $(size(n)) Vector do not form a compatible NArray"
        new(a,n)
    end
end
NArray(a::Array{T,N},n::Vector{C}) where {T,N,C} = NArray{T,N,C}(a,n)


import Base: show,setindex!,getindex,view,size,eltype,ndims,cat,repeat,names
function show(io::IO,an::NArray)
    Base.print(io,join(["$x " for x in an.names]),"\n")
    Base.showarray(io,an.array,false)
end
show(io::IO, ::MIME"text/plain", an::NArray{T,N,C}) where {T,N,C} = print(io, "NArray{$T,$N,$C}:\n", an)

setindex!(an::NArray{T,N,C},v,ind::C) where {T,N,C} = Base.setindex!(an.array,v,colonsind(an,ind)...)
getindex(an::NArray{T,N,C},ind::C) where {T,N,C} = Base.slicedim(an.array,N,findindex(an,ind))
view(an::NArray{T,N,C},ind::C) where {T,N,C} = Base.view(an.array,colonsind(an,ind)...)

setindex!(an::NArray{T,N,C},v,ind::AbstractString) where {T,N,C<:Column} = Base.setindex!(an.array,v,colonsind(an,ind)...)
getindex(an::NArray{T,N,C},ind::AbstractString) where {T,N,C<:Column} = Base.slicedim(an.array,N,findindex(an,ind))
view(an::NArray{T,N,C},ind::AbstractString) where {T,N,C<:Column} = Base.view(an.array,colonsind(an,ind)...)

getindex(an::NArray{T,N,C},ind::Array{C}) where {T,N,C} = Base.slicedim(an.array,N,findindex.(an,ind))
getindex(an::NArray{T,N,C},ind::Array{S}) where {T,N,C<:Column,S<:AbstractString} = Base.slicedim(an.array,N,findindex.(an,ind))

getindex(an::NArray{T,N,C},ind) where {T,N,C}         =getindex(an.array,ind)
getindex(an::NArray{T,2,C},i1,i2) where {T,C}         =getindex(an.array,i1,i2)
getindex(an::NArray{T,3,C},i1,i2,i3) where {T,C}      =getindex(an.array,i1,i2,i3)
getindex(an::NArray{T,4,C},i1,i2,i3,i4) where {T,C}   =getindex(an.array,i1,i2,i3,i4)
setindex!(an::NArray{T,N,C},v,ind) where {T,N,C}      =setindex!(an.array,v,ind)
setindex!(an::NArray{T,2,C},v,i1,i2) where {T,C}      =setindex!(an.array,v,i1,i2)
setindex!(an::NArray{T,3,C},v,i1,i2,i3) where {T,C}   =setindex!(an.array,v,i1,i2,i3)
setindex!(an::NArray{T,4,C},v,i1,i2,i3,i4) where {T,C}=setindex!(an.array,v,i1,i2,i3,i4)

function getindex(an::NArray{T,N,C},ind::BitArray{1}) where {T,N,C}
    @assert length(ind)==size(an,N)
    NArray{T,N,C}(Base.slicedim(an.array,N,ind),an.names[ind])
end

findindex(an::NArray{T,N,C},nm::C) where {T,N,C} = findfirst(an.names .== nm)
findindex(an::NArray{T,N,C},nm::AbstractString) where {T,N,C<:Column} = findfirst( name.(an.names) .== nm )

colonsind(an::NArray{T,1},nm) where T     = ( findindex(an,nm), )
colonsind(an::NArray{T,N},nm) where {T,N} = ( ntuple(n->Colon(),N-1)..., findindex(an,nm) )

hasname(an::NArray,nm)=findindex(an,nm)>0
getnames(an::NArray)=an.names
names(an::NArray)=an.names
getarray(an::NArray)=an.array
array(an::NArray)=an.array
#getname(an::NArray,nm)=an[nm]
function addname!(an::NArray{T,N,C},newcol::Array{R,M},newname::C) where {T,R,N,M,C}
    @assert N==M+1 "The new named 'column' must be an array of one less dimension than the NArray"
    T==promote_type(T,R) || warn("watch for possible down-conversion errors")
    @assert size(an.array,1:M...)==size(newcol) "The new named 'column' must match the shape of the NArray 'columns'"
    an.array=cat(N,an.array,newcol)
    an.names=vcat(an.names,newname)
    return an
end
function replacename!(an::NArray{T,N,C},oldname::C,newname::C,newcol=an[oldname]) where {T,N,C}
    idx=findindex(an,oldname)
    @assert idx>0 "The named `column` to be replaced must be present in the input NArray"
    an[oldname]=newcol # an[idx] would refer to a single element of an.array, not a full column
    an.names[idx]=newname
    return an
end


size(an::NArray)=Base.size(an.array)
size(an::NArray,i::Integer...)=Base.size(an.array,i...)
eltype(::NArray{T}) where T=T
ndims(::NArray{T,N}) where {T,N}=N
nametype(::NArray{T,N,C}) where {T,N,C}=C

function cat(d::Integer,a::NArray{T,N,C},b::NArray{T,N,C}) where {T,N,C}
    @assert 0<d<=N "NArray objects can not (easily) be concatinated beyond their named dimension"
    if N==d # we're adding columns, make sure none are repeated
        any([any(a.names.==x) for x in b.names]) && error("Can not append matching names!")
        out = NArray{T,N,C}(Base.cat(N,a.array,b.array),vcat(a.names,b.names))
    else # we're adding *to* the named dimension, so all names must match in order
        @assert length(a.names)==length(b.names) && all([a.names[i]==b.names[i] for i=1:length(a.names)]) "Named dimensions must be identical"
        out = NArray{T,N,C}(Base.cat(d,a.array,b.array),copy(a.names)) # let the standard cat figure out if dimensions match
    end
    return out
end
cat(d::Integer,a::NArray{T,N,C},b::NArray{T,N,C}...) where {T,N,C} = Base.cat(d,Base.cat(d,a,b[1]),b[2:end]...)

function repeat(a::NArray{T,N,C};outer=(1:N-1...)) where {T,N,C}
    # Only allowing for repeating of the non-named dimensions requires that
    # we assume the outer specification has left it off
    lo=length(outer)
    @assert lo>=N-1 "Number of outer repetitions ($lo) can no be less than dimensions-1 of input ($N)"
    newarray= repeat(a.array,outer=ones(Int,lo+1)) # increase the dimensionality, as necessary
    if lo>N-1 # we need to play with permutedims in order to get this right
        newarray= permutedims(newarray,([1:N-1;N+1:lo+1]...,N)) # permute N into the last dimension
    end
    newarray=repeat(newarray,outer=(outer...,1))
    NArray(newarray,a.names) # no {T,N,C} signature because N might be wrong after repetition
end
