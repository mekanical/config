#!/usr/bin/ruby
c = 2.99792458e8
print("Calc. Wavelength\n")
print("input frequency[GHz] : ")
f = (gets.to_f)*1.0e9
print("input epsilon R : ")
epsR = gets.to_f

lambda  = c/f/Math::sqrt(epsR)
print("lambda   = ", lambda*1.0e3, "[mm]\n")
print("lambda/2 = ", lambda/2.0*1.0e3, "[mm]\n")
print("lambda/4 = ", lambda/4.0*1.0e3, "[mm]\n")
