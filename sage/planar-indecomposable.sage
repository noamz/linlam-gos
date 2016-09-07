# generating function for planar indecomposable terms

R.<x,z> = ZZ[]

max = 30
B = Sequence([0..max],R)

for i in range(max):
    B[i+1] = x + z*(B[i]-B[i].subs(x=0))^2 + z*(B[i] - B[i].subs(x=0))/x
    B[i+1] = B[i+1].truncate(z,max)
   
B[max].subs(x=0)
# 428157758086840320*z^39 + 36143187370967040*z^37 + 3073132646563840*z^35 + 263411369705472*z^33 + 22783499599872*z^31 + 1990947110912*z^29 + 176028860416*z^27 + 15774990336*z^25 + 1436098560*z^23 + 133186560*z^21 + 12629760*z^19 + 1230592*z^17 + 124032*z^15 + 13056*z^13 + 1456*z^11 + 176*z^9 + 24*z^7 + 4*z^5 + z^3 + z

B[max].truncate(z,max/2).polynomial(z)
# (1767263190*x^20 + 8597496600*x^18 + 19234572480*x^16 + 26209512000*x^14 + 24272504256*x^12 + 16128459776*x^10 + 7898542080*x^8 + 2870816256*x^6 + 760032000*x^4 + 133186560*x^2 + 12629760)*z^19 + (477638700*x^19 + 2203961430*x^17 + 4653525600*x^15 + 5949143200*x^13 + 5131782656*x^11 + 3147075360*x^9 + 1404539136*x^7 + 456019200*x^5 + 103334400*x^3 + 12629760*x)*z^18 + (129644790*x^18 + 565722720*x^16 + 1123264800*x^14 + 1341261376*x^12 + 1071584800*x^10 + 601945344*x^8 + 242161920*x^6 + 68889600*x^4 + 12629760*x^2 + 1230592)*z^17 + (35357670*x^17 + 145422675*x^15 + 270415600*x^13 + 300043744*x^11 + 220540320*x^9 + 112432320*x^7 + 40185600*x^5 + 9715200*x^3 + 1230592*x)*z^16 + (9694845*x^16 + 37442160*x^14 + 64899744*x^12 + 66512160*x^10 + 44616000*x^8 + 20401920*x^6 + 6347264*x^4 + 1230592*x^2 + 124032)*z^15 + (2674440*x^15 + 9657700*x^13 + 15519504*x^11 + 14586000*x^9 + 8840832*x^7 + 3570336*x^5 + 936320*x^3 + 124032*x)*z^14 + (742900*x^14 + 2496144*x^12 + 3695120*x^10 + 3157440*x^8 + 1707552*x^6 + 595840*x^4 + 124032*x^2 + 13056)*z^13 + (208012*x^13 + 646646*x^11 + 875160*x^9 + 672672*x^7 + 319200*x^5 + 93024*x^3 + 13056*x)*z^12 + (58786*x^12 + 167960*x^10 + 205920*x^8 + 140448*x^6 + 57120*x^4 + 13056*x^2 + 1456)*z^11 + (16796*x^11 + 43758*x^9 + 48048*x^7 + 28560*x^5 + 9600*x^3 + 1456*x)*z^10 + (4862*x^10 + 11440*x^8 + 11088*x^6 + 5600*x^4 + 1456*x^2 + 176)*z^9 + (1430*x^9 + 3003*x^7 + 2520*x^5 + 1040*x^3 + 176*x)*z^8 + (429*x^8 + 792*x^6 + 560*x^4 + 176*x^2 + 24)*z^7 + (132*x^7 + 210*x^5 + 120*x^3 + 24*x)*z^6 + (42*x^6 + 56*x^4 + 24*x^2 + 4)*z^5 + (14*x^5 + 15*x^3 + 4*x)*z^4 + (5*x^4 + 4*x^2 + 1)*z^3 + (2*x^3 + x)*z^2 + (x^2 + 1)*z + x
