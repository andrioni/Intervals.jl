## Intervals.jl

#### Interval arithmetic library for Julia

This is a work-in-progress Julia package that reimplements [MPFI](http://perso.ens-lyon.fr/nathalie.revol/software.html).
in pure Julia. The goal is to provide an interface as close as possible of 
[MPFI.jl](https://github.com/andrioni/MPFI.jl), in order to allow users to 
quickly migrate code between the two packages if the need arises.

##### Example

```julia
# For convenience, let's just use 53 bits (as a Float64)
julia> set_bigfloat_precision(53)
53

# The following creates an interval centered on 1.1.
# Since 1.1 isn't exactly representable as a floating-point number,
# the shortest interval that includes it is returned.
julia> x = Interval("1.1")
[1.0999999999999999e+00, 1.1000000000000001e+00] with 53 bits of precision

# It is also possible to create an interval through its endpoints.
julia> Interval("1", "2")
[1e+00, 2e+00] with 53 bits of precision

julia> x + y
[2.0999999999999996e+00, 3.1000000000000001e+00] with 53 bits of precision

# Intervals with Float32 or Float64 endpoints are also available
julia> x = Interval(2.5)
[2.5, 2.5]

julia> typeof(x)
Interval{Float64} (constructor with 2 methods)

julia> x = Interval(1f0, 6.125f0)
[1.0, 6.125]

julia> typeof(x)
Interval{Float32} (constructor with 2 methods)
```
