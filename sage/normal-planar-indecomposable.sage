# generating functions for neutral/normal planar indecomposable terms

R.<x,z> = ZZ[]

max = 20
B = Sequence([0..max],R)
C = Sequence([0..max],R)

for i in range(max):
    B[i+1] = x + B[i]*(C[i] - C[i].subs(x=0))
    B[i+1] = B[i+1].truncate(z,max)
    C[i+1] = z*B[i+1] + (C[i] - C[i].subs(x=0))/x
    C[i+1] = C[i+1].truncate(z,max)
   
C[max].subs(x=0)
# 230434387370*z^19 + 67954785032*z^18 + 19222812259*z^17 + 5211420891*z^16 + 1363054155*z^15 + 347106259*z^14 + 86552545*z^13 + 20986456*z^12 + 4732628*z^11 + 857956*z^10 + 118668*z^9 + 16965*z^8 + 2530*z^7 + 399*z^6 + 68*z^5 + 13*z^4 + 3*z^3 + z^2 + z

B[max].truncate(z,max/2).polynomial(z)
# (4862*x^10 + 19448*x^9 + 45045*x^8 + 78936*x^7 + 115500*x^6 + 147420*x^5 + 166257*x^4 + 161820*x^3 + 118668*x^2)*z^9 + (1430*x^9 + 5005*x^8 + 10296*x^7 + 16170*x^6 + 21252*x^5 + 24150*x^4 + 23400*x^3 + 16965*x^2)*z^8 + (429*x^8 + 1287*x^7 + 2310*x^6 + 3192*x^5 + 3675*x^4 + 3542*x^3 + 2530*x^2)*z^7 + (132*x^7 + 330*x^6 + 504*x^5 + 595*x^4 + 570*x^3 + 399*x^2)*z^6 + (42*x^6 + 84*x^5 + 105*x^4 + 100*x^3 + 68*x^2)*z^5 + (14*x^5 + 21*x^4 + 20*x^3 + 13*x^2)*z^4 + (5*x^4 + 5*x^3 + 3*x^2)*z^3 + (2*x^3 + x^2)*z^2 + x^2*z + x