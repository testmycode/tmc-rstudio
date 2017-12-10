#8)
#a)
# Matriisit A ja B voidaan maarittaa esimerkiksi komennoilla
A <- matrix(c(seq(2, 16, by = 2), seq(1, 15, by = 2)), nrow = 4)
B <- diag(1:4)
# Huomaa, etta matriisitulon operaattori on %*%, ei pelkka *.
C <- A%*%B

#b)
#D <- C[-3, -1]
# tai
D <- C[c(T, T, F, T), c(F, T, T, T)]

#c)
#Tallennetaan matriisiin muuttujaan E D:n sijasta, testauksen vuoksi.
E <- cbind(D, c(3, -2, 0))

#d)
#Tallennetaan matriisi muuttujaan G D:n sijasta, testauksen vuoksi.
G <- rbind(E, c(-4, 1, 1, 2))

#e)
#Tallennetaan matriisi muuttujaan tulostuksen sijaan.
#TODO: tarkista tulostuksesta
H <- solve(G)
