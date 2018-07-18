export Column, addunit, addunit!, name, unit, addfunction, plainstring, @c_str
struct Column
    name::AbstractString
    unit::AbstractString
    power::Int64
    value::Real
    norm::Nullable{Column}
    func::Nullable{AbstractString}
end

Column(a::Column;norm::Nullable{Column}=Nullable{Column}(),func::Nullable{AbstractString}=Nullable{AbstractString}())=Column(a.name,a.unit,a.power,a.value,norm,func)
function Column(name::AbstractString;unit::AbstractString="",power::Integer=0,value::Real=1,norm::Nullable{Column}=Nullable{Column}(),func::Nullable{AbstractString}=Nullable{AbstractString}())
    Column(name,unit,power,value,norm,func)
end
function showcolumn(io::IO,a::Column;compact::Bool=false,isnorm::Bool=false)
  hasfunc=~isnull(a.func)
  hasfunc && print(io,get(a.func),"(")
    if isnorm
        normval = a.value!=1; normpow = a.power!=0
        normval && print(io,"$(a.value)"*(normpow?"×":" "))
        normpow && print(io,"10",number2exponent(a.power)," ")
        print(io,a.name)
        isempty(a.unit)||print(io," ",a.unit)
    else
        print(io,a.name)
    end
    if !isnorm && !compact
        hasunit=!isempty(a.unit)
        hasnorm=!isnull(a.norm)
        hasunit&&!hasnorm&&print(io,"/")
        !hasunit&&hasnorm&&print(io," per ")
        hasunit&&hasnorm&&print(io," [")
        if hasunit
            v=a.value; p=a.power
            if !haskey(powerprefix,p)
                pk=collect(keys(powerprefix))
                i=indmin(abs(pk.-p))
                newp=pk[i]
                v*=newp-p
                p=newp
            end
            v != 1 && print(io,v," ")
            print(io,powerprefix[p],a.unit)
        end
        hasnorm&&hasunit&&print(io,"/")
        hasnorm&&showcolumn(io,get(a.norm);isnorm=true)
        hasunit&&hasnorm&&print(io,"]")
    end
    hasfunc && print(io,")")
end
global _Column_unitcolor=:yellow
global _Column_normcolor=:blue
set_unitcolor(a::Symbol)=global _Column_unitcolor=a
set_normcolor(a::Symbol)=global _Column_normcolor=a
function showcolumn_with_color(io::IO,a::Column;compact::Bool=false,isnorm::Bool=false,unitcolor::Symbol=_Column_unitcolor)
  hasfunc=~isnull(a.func)
  hasfunc && print(io,get(a.func),"(")
    if isnorm
        normval = a.value!=1; normpow = a.power!=0
        normval && print_with_color(unitcolor,io,"$(a.value)"*(normpow?"×":" "))
        normpow && print_with_color(unitcolor,io,"10",number2exponent(a.power)," ")
        print_with_color(unitcolor,io,a.name)
        isempty(a.unit)||print_with_color(unitcolor,io," ",a.unit)
    else
        print(io,a.name)
    end
    if !isnorm && !compact
        hasunit=!isempty(a.unit)
        hasnorm=!isnull(a.norm)
        hasunit&&!hasnorm&&print_with_color(unitcolor,io,"/")
        !hasunit&&hasnorm&&print_with_color(unitcolor,io," per ")
        hasunit&&hasnorm&&print_with_color(unitcolor,io," [")
        if hasunit
            v=a.value; p=a.power
            if !haskey(powerprefix,p)
                pk=collect(keys(powerprefix))
                i=indmin(abs(pk.-p))
                newp=pk[i]
                v*=newp-p
                p=newp
            end
            v != 1 && print_with_color(unitcolor,io,v," ")
            print_with_color(unitcolor,io,powerprefix[p],a.unit)
        end
        hasnorm&&hasunit&&print_with_color(unitcolor,io,"/")
        hasnorm&&showcolumn_with_color(io,get(a.norm);isnorm=true,unitcolor=_Column_normcolor)
        hasunit&&hasnorm&&print_with_color(unitcolor,io,"]")
    end
    hasfunc && print(io,")")
end
Base.show(io::IO,a::Column)=Base.have_color?showcolumn_with_color(io,a;compact=false):showcolumn(io,a;compact=false)
Base.showcompact(io::IO,a::Column)=showcolumn(io,a;compact=true)
plainstring(a::Column;k...)=(io=IOBuffer();showcolumn(io,a;k...);String(take!(io)))

Base.:/(a::Column,b::Column)=Column(a,norm=Nullable{Column}(b))

Column{T<:AbstractString}(cv::Array{T,1};k...)=Column[Column(x;k...) for x in cv]
function addunit(a::Column,unit::AbstractString;power::Integer=a.power,value::Real=a.value,norm::Nullable{Column}=a.norm,func::Nullable{AbstractString}=a.func)
    Column(a.name,unit,power,value,norm,func)
end
function addunit{T<:Column,S<:AbstractString}(cv::Array{T,1},uv::Array{S,1};k...)
    @assert length(cv)==length(uv)
    Column[ addunit(c,u;k...) for (c,u) in zip(cv,uv)]
end
function addunit!{T<:Column,S<:AbstractString}(cv::AbstractArray{T},uv::AbstractArray{S};k...)
    @assert length(cv)==length(uv)
    for i=1:length(cv); cv[i]=addunit(cv[i],uv[i];k...); end
end
addunit!{T<:Column}(cv::AbstractArray{T},u::AbstractString;k...)=addunit!(cv,repeat([u],outer=[length(cv)]);k...)

name(a::Column)=a.name
# name(a)=a
unit(a::Column)=a.unit
# for x in (:name,:unit)
#     @eval $x{T<:Column}(av::Array{T})=[$x(a) for a in av]
# end

const powerprefix= Dict(i=>k for (i,k) in zip([12,9,6,3,0,-2,-3,-6,-9,-12,-15],["T","G","M","k","","c","m","μ","n","p","f"]) )

number2exponent(a::Integer)=number2exponent("$a")
const _nstrings=["0","1","2","3","4","5","6","7","8","9","-","+"]
const _pstrings=["⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹","⁻","⁺"]
function replaceeach(s::AbstractString,a,b)
    @assert length(a)==length(b)
    for i=1:length(a);s=replace(s,a[i],b[i]);end
    return s
end
number2exponent(a::AbstractString)=replaceeach(a,_nstrings,_pstrings)
function exponent2number(a::AbstractString)
    # take out any ×10 since they're guaranteed(?) to have a prefactor
    a=replaceeach(a,["×10"],["E"]) #
    # then deal with remaining 10ˣ (where x=0-9+\-), which need a leading 1
    a=replaceeach(a,"10".*_pstrings,"1E".*_nstrings)
    # finally we can take out any remaining exponents
    return replaceeach(a,_pstrings,_nstrings)
end


addfunction(c::Column,fn::Function)=addfunction(c,Symbol(fn))
function addfunction(c::Column,fn::Symbol)
  sf=[:log10] # add more special functions as necessary
  rt=["log₁₀"]
  i=findfirst(fn.==sf)
  addfunction(c,i>0?rt[i]:String(fn))
end
addfunction(c::Column,fn::AbstractString)=Column(c.name,c.unit,c.power,c.value,c.norm,Nullable{AbstractString}(fn))


macro c_str(p)
    parseColumn(p)
end
function parseValueUnit(p::AbstractString)
    s=split(p,' ') # split /value unit into [value,unit] or just [unit,] if value is not present
    valstr=length(s)>1 ? s[1] : "1"
    value= isa(parse(valstr),Number) ? parse(valstr) : 1
    unit=s[end]
    return (value,unit)
end
function parseNorm(p::AbstractString)
    s=split(p,' ') # might be "1E5 monitor counts" or "monitor counts" or "m4", etc.
    hasvalue = isa(parse(s[1]),Number)
    value = hasvalue ? parse(s[1]) : 1
    i=hasvalue ? 1 : 0
    name=s[i+1]
    length(s)>(i+1) ? Column(name,value=value,unit=s[i+2]) : Column(name,value=value)
end
function parseColumn(p::AbstractString)
    # we might have been passed "NAME", "NAME/UNIT", "NAME [UNIT/NORMNAME]", or "NAME [UNIT/NORMNAME NORMUNIT]"
    #rf=r"^([a-zA-Z0-9₁₀]+)\((.+)\)$"
    rf=r"^(.+)\((.+)\)$" # match anything(anything) where anything is one or more character of any type
    hasfun=ismatch(rf,p)
    if hasfun
        m=match(rf,p).captures
        fun=m[1];p=m[2]
    end
    p=exponent2number(p) # (try to) ensure we go from a pretty unicode string to a simple ascii string
    r0=r"^([[:alnum:]]+)(.*)$"
    ismatch(r0,p)||error("Can not parse \"$p\" as a valid Column")
    m_name=match(r0,p).captures
    name=m_name[1]; p=m_name[2] # split into the name and whatever is left
    isempty(p)&&(return Column(name))
    hasnorm= p[1]==' ' # either " per ..." or " [...]"
    hasunit= hasnorm ? p[2]=='[' : p[1]=='/' # either " [...]" or !hasnorm
    if !hasnorm && hasunit
        (value,unit)=parseValueUnit(p[2:end]) # p = "/..."
    elseif !hasunit && hasnorm
        norm=parseNorm(split(p," per ",keep=false))
    elseif hasunit && hasnorm
        s=split(strip(p,Set(['[',']',' '])),"/") # go from " [unit/val norm]" to "unit/val norm" to ["unit","val norm"]
        (value,unit)=parseValueUnit(s[1])
        norm=parseNorm(s[2])
    else
        error("Unexpected unit/norm specifier `$p`")
    end
    col = hasunit ? Column(name,unit=unit,value=value) : Column(name)
    hasnorm && (col/=norm)
    return hasfun ? addfunction(col,fun) : col
end

_safevalue(a::Column)= a.value * (a.power<0?1/10^abs(a.power):10^a.power)
import Base: ==
function ==(a::Column,b::Column)
    ie=all([a.name,a.unit,_safevalue(a)].==[b.name,b.unit,_safevalue(b)])
    ne =!isnull(a.norm)&&!isnull(b.norm) && get(a.norm)==get(b.norm)
    ne|= isnull(a.norm)&& isnull(b.norm) # if both are null they are equal
    fe =!isnull(a.func)&&!isnull(b.func) && get(a.func)==get(b.func)
    fe|= isnull(a.func)&& isnull(b.func)
    return ie&ne&fe
end
