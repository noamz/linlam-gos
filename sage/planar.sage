# generating function for planar terms

R.<x,z> = ZZ[]

max = 40
B = Sequence([0..max],R)

for i in range(max):
    B[i+1] = x + z*B[i]^2 + z*(B[i] - B[i].subs(x=0))/x
    B[i+1] = B[i+1].truncate(z,max)
   
B[max].subs(x=0)
# 8138178004138611179520*z^39 + 444370175232646840320*z^37 + 24428493151359467520*z^35 + 1353050213048123392*z^33 + 75576645116559360*z^31 + 4261707069259776*z^29 + 242919827374080*z^27 + 14018773254144*z^25 + 820675092480*z^23 + 48855252992*z^21 + 2966845440*z^19 + 184549376*z^17 + 11824384*z^15 + 786432*z^13 + 54912*z^11 + 4096*z^9 + 336*z^7 + 32*z^5 + 4*z^3 + z

B[max].truncate(z,max/2).polynomial(z)
# (1767263190*x^20 + 17672631900*x^18 + 79342611480*x^16 + 210959827200*x^14 + 368846878400*x^12 + 444975218688*x^10 + 377649043200*x^8 + 225144471552*x^6 + 91975564800*x^4 + 23991418880*x^2 + 2966845440)*z^19 + (477638700*x^19 + 4537567650*x^17 + 19234572480*x^15 + 47925964800*x^13 + 77793159808*x^11 + 86081515520*x^9 + 65927347200*x^7 + 34637611008*x^5 + 11981491200*x^3 + 2399141888*x)*z^18 + (129644790*x^18 + 1166803110*x^16 + 4653525600*x^14 + 10816624000*x^12 + 16202362176*x^10 + 16310181888*x^8 + 11139448320*x^6 + 5060689920*x^4 + 1426368000*x^2 + 184549376)*z^17 + (35357670*x^17 + 300540195*x^15 + 1123264800*x^13 + 2422923776*x^11 + 3325608000*x^9 + 3015327744*x^7 + 1808970240*x^5 + 692060160*x^3 + 148342272*x)*z^16 + (9694845*x^16 + 77558760*x^14 + 270415600*x^12 + 538009472*x^10 + 670956000*x^8 + 541212672*x^6 + 279568128*x^4 + 86507520*x^2 + 11824384)*z^15 + (2674440*x^15 + 20058300*x^13 + 64899744*x^11 + 118243840*x^9 + 132612480*x^7 + 93671424*x^5 + 40517120*x^3 + 9437184*x)*z^14 + (742900*x^14 + 5200300*x^12 + 15519504*x^10 + 25671360*x^8 + 25561536*x^6 + 15482880*x^4 + 5374720*x^2 + 786432)*z^13 + (208012*x^13 + 1352078*x^11 + 3695120*x^9 + 5491200*x^7 + 4775232*x^5 + 2408448*x^3 + 622336*x)*z^12 + (58786*x^12 + 352716*x^10 + 875160*x^8 + 1153152*x^6 + 856800*x^4 + 344064*x^2 + 54912)*z^11 + (16796*x^11 + 92378*x^9 + 205920*x^7 + 236544*x^5 + 145600*x^3 + 43008*x)*z^10 + (4862*x^10 + 24310*x^8 + 48048*x^6 + 47040*x^4 + 22880*x^2 + 4096)*z^9 + (1430*x^9 + 6435*x^7 + 11088*x^5 + 8960*x^3 + 3168*x)*z^8 + (429*x^8 + 1716*x^6 + 2520*x^4 + 1600*x^2 + 336)*z^7 + (132*x^7 + 462*x^5 + 560*x^3 + 256*x)*z^6 + (42*x^6 + 126*x^4 + 120*x^2 + 32)*z^5 + (14*x^5 + 35*x^3 + 24*x)*z^4 + (5*x^4 + 10*x^2 + 4)*z^3 + (2*x^3 + 3*x)*z^2 + (x^2 + 1)*z + x