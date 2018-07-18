using SimpleNamedArrays
using Base.Test

# First, test the Column type
d=Column("detector",unit="counts")
n=Column("monitor",unit="counts",value=5,power=3)
dn=d/n
c=c"detector [counts/5×10³ monitor counts]"
@test c==dn
@test !(c===dn)

# Now test NArray:
