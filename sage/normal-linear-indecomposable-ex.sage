# generating functions for neutral/normal indecomposable linear terms modulo free exchange

R.<x,z> = ZZ[]

max = 20
B = Sequence([0..max],R)
C = Sequence([0..max],R)

for i in range(max):
    B[i+1] = x + B[i]*(C[i]-C[i].subs(x=0))
    B[i+1] = B[i+1].truncate(z,max)
    C[i+1] = z*sum(derivative(B[i+1],x,k)/factorial(k) for k in range(i+1))
    C[i+1] = C[i+1].truncate(z,max)
   
C[max].subs(x=0)
# 2796475872605709079512*z^19 + 75197869250518812754*z^18 + 2136017303903513184*z^17 + 64291032462761955*z^16 + 2057490936366320*z^15 + 70282204726396*z^14 + 2573779506192*z^13 + 101551822350*z^12 + 4342263000*z^11 + 202601898*z^10 + 10401712*z^9 + 593859*z^8 + 38232*z^7 + 2830*z^6 + 248*z^5 + 27*z^4 + 4*z^3 + z^2 + z

B[max].truncate(z,max/2).polynomial(z)
# (4862*x^10 + 82452*x^9 + 684950*x^8 + 3592384*x^7 + 12877684*x^6 + 32162640*x^5 + 55009212*x^4 + 60849248*x^3 + 37338466*x^2)*z^9 + (1430*x^9 + 19898*x^8 + 133864*x^7 + 556952*x^6 + 1535708*x^5 + 2815396*x^4 + 3270600*x^3 + 2067864*x^2)*z^8 + (429*x^8 + 4760*x^7 + 25092*x^6 + 79472*x^5 + 159318*x^4 + 196352*x^3 + 128436*x^2)*z^7 + (132*x^7 + 1124*x^6 + 4436*x^5 + 10068*x^4 + 13372*x^3 + 9100*x^2)*z^6 + (42*x^6 + 260*x^5 + 718*x^4 + 1056*x^3 + 754*x^2)*z^5 + (14*x^5 + 58*x^4 + 100*x^3 + 76*x^2)*z^4 + (5*x^4 + 12*x^3 + 10*x^2)*z^3 + (2*x^3 + 2*x^2)*z^2 + x^2*z + x

C[max].subs(x=1) - C[max].subs(x=0)
# 103469607286411235941944*z^19 + 2631925423768158446390*z^18 + 70488571028815935072*z^17 + 1993022006345620605*z^16 + 59667237154623280*z^15 + 1897619527612692*z^14 + 64344487654800*z^13 + 2335691914050*z^12 + 91187523000*z^11 + 3849436062*z^10 + 176829104*z^9 + 8907885*z^8 + 497016*z^7 + 31130*z^6 + 2232*z^5 + 189*z^4 + 20*z^3 + 3*z^2 + z
