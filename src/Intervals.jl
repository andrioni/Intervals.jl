# This file is part of Intervals.jl.
#
# Intervals.jl is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# Intervals.jl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Intervals.jl; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
# Also add information on how to contact you by electronic and paper mail.
#
# Copyright (C) 2013 Alessandro Andrioni


module Intervals

export
    Interval,
    # bisect,
    # blow,
    # diam,
    # diam_abs,
    # diam_rel,
    # mag,
    mid
    # mig,
    # isbounded

import
    Base: precision, string, print, show, showcompact, promote_rule,
        promote, convert, +, *, -, /, exp, isinf, isnan, nan, inf, sqrt,
        square, exp, exp2, expm1, cosh, sinh, tanh, sech, csch, coth, inv,
        sqrt, cbrt, abs, log, log2, log10, log1p, sin, cos, tan, sec,
        csc, acos, asin, atan, acosh, asinh, atanh, isempty, union,
        intersect, in, cmp, inv

typealias IntervalTypes Union(Float32, Float64, BigFloat)
typealias SmallFloat Union(Float32, Float64)

immutable Interval{T<:IntervalTypes} <: Number
    left::T
    right::T

    function Interval(left::String, right::String)
        left = parsedown(T, left)
        right = parseup(T, right)
        if left > right
            left, right = right, left
        end
        new(left, right)
    end
    function Interval(left::T, right::T)
        if left > right
            left, right = right, left
        end
        new(left, right)
    end
end

# Monadic constructors
Interval(x::Interval) = x
Interval{T<:IntervalTypes}(x::T) = Interval{T}(x, x)
Interval(x::Real) = Interval(convert(FloatingPoint, x))
Interval(x::String) = Interval{BigFloat}(x, x)
function Interval(x::Rational)
    left = with_rounding(RoundDown) do
        with_bigfloat_rounding(RoundDown) do
            num(x) / den(x)
        end
    end
    right = with_rounding(RoundUp) do
        with_bigfloat_rounding(RoundUp) do
            num(x) / den(x)
        end
    end
    Interval(left, right)
end

# Dyadic constructors
Interval{T<:IntervalTypes}(left::T, right::T) = Interval{T}(left, right)
function Interval{T<:Real}(left::T, right::T)
    Interval(convert(FloatingPoint, left), convert(FloatingPoint, right))
end
function Interval(left::Real, right::Real)
    Interval(promote(left, right)...)
end
function Interval(left::String, right::String)
    Interval{BigFloat}(left, right)
end

# Converstion and promotion related functions
# Conversions to Interval
convert(::Type{Interval}, x::Rational) = Interval(x)
convert(::Type{Interval}, x::Real) = Interval(x)

# Conversions from Interval
convert(::Type{BigFloat}, x::Interval) = convert(BigFloat, mid(x))
convert(::Type{Float64}, x::Interval) = convert(Float64, mid(x))
convert(::Type{Float32}, x::Interval) = convert(Float32, mid(x))

for to in (Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64, BigInt, Float32)
    @eval begin
        function convert(::Type{$to}, x::Interval)
            convert($to, convert(BigFloat, x))
        end
    end
end
convert(::Type{Integer}, x::Interval) = convert(BigInt, x)
convert(::Type{FloatingPoint}, x::Interval) = convert(BigFloat, x)

# Basic operations
function +(x::Interval, y::Interval)
    l = roundop(+, RoundDown, x.left, y.left)
    r = roundop(+, RoundUp, x.right, y.right)
    Interval(l, r)
end
function -(x::Interval, y::Interval)
    l = roundop(-, RoundDown, x.left, y.right)
    r = roundop(-, RoundUp, x.right, y.left)
    Interval(l, r)
end
function *(x::Interval, y::Interval)
    if x.left >= 0
        if y.left >= 0
            l = roundop(*, RoundDown, x.left, y.left)
            r = roundop(*, RoundUp, x.right, y.right)
        elseif y.right <= 0
            l = roundop(*, RoundDown, x.right, y.left)
            r = roundop(*, RoundUp, x.left, y.right)
        else
            l = roundop(*, RoundDown, x.right, y.left)
            r = roundop(*, RoundUp, x.right, y.right)
        end
    elseif x.right <= 0
        if y.left >= 0
            l = roundop(*, RoundDown, x.left, y.right)
            r = roundop(*, RoundUp, x.right, y.left)
        elseif y.right <= 0
            l = roundop(*, RoundDown, x.right, y.right)
            r = roundop(*, RoundUp, x.left, y.left)
        else
            l = roundop(*, RoundDown, x.left, y.right)
            r = roundop(*, RoundUp, x.left, y.left)
        end
    else
        if y.left >= 0
            l = roundop(*, RoundDown, x.left, y.right)
            r = roundop(*, RoundUp, x.right, y.right)
        elseif y.right <= 0
            l = roundop(*, RoundDown, x.right, y.left)
            r = roundop(*, RoundUp, x.left, y.left)
        else
            l = min(roundop(*, RoundDown, x.left, y.right),
                    roundop(*, RoundDown, x.right, y.left))
            r = max(roundop(*, RoundUp, x.left, y.left),
                    roundop(*, RoundUp, x.right, y.right))
        end
    end
    Interval(l, r)
end
# TODO: Reimplement directly without using inv
function /(x::Interval, y::Interval)
    i = inv(y)
    x * i
end

# General functions
function mid(x::Interval)
    m = x.left + x.right
    if isinf(m)
        return (x.left / 2) + (x.right / 2)
    else
        return m / 2
    end
end

function inv{T<:SmallFloat}(x::Interval{T})
    l = roundop(/, RoundDown, 1., x.right)
    r = roundop(/, RoundUp, 1., x.left)
    Interval(l, r)
end
# TODO: Call MPFR inv directly/write without roundop
function inv(x::Interval{BigFloat})
    i = BigFloat(1)
    l = roundop(/, RoundDown, i, x.right)
    r = roundop(/, RoundUp, i, x.left)
    Interval(l, r)
end

# Printing-related functions
string(x::Interval) = "[$(string(x.left)), $(string(x.right))]"
print(io::IO, x::Interval) = print(io, string(x))
show(io::IO, x::Interval) = print(io, string(x))
show(io::IO, x::Interval{BigFloat}) = print(io, string(x), " with $(precision(x.left)) bits of precision")
showcompact(io::IO, x::Interval) = print(io, string(x))

# Internal utility functions and macros
function parseup(::Type{Float32}, x::String)
    with_rounding(RoundUp) do
        float32(x)
    end
end
function parseup(::Type{Float64}, x::String)
    with_rounding(RoundUp) do
        float64(x)
    end
end
function parseup(::Type{BigFloat}, x::String)
    with_bigfloat_rounding(RoundUp) do
        BigFloat(x)
    end
end
function parsedown(::Type{Float32}, x::String)
    with_rounding(RoundDown) do
        float32(x)
    end
end
function parsedown(::Type{Float64}, x::String)
    with_rounding(RoundDown) do
        float64(x)
    end
end
function parsedown(::Type{BigFloat}, x::String)
    with_bigfloat_rounding(RoundDown) do
        BigFloat(x)
    end
end
# TODO: Use macros instead?
function roundop(f, r, x::SmallFloat, y::SmallFloat)
    with_rounding(r) do
        f(x, y)
    end
end
function roundop(f, r, x::BigFloat, y::BigFloat)
    with_bigfloat_rounding(r) do
        f(x, y)
    end
end

end # module
