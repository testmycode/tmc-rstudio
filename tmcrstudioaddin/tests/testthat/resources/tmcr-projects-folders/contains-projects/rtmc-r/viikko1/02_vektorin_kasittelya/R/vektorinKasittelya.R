#7)
#a)
# Kyseessa on modulo- eli jakojaannosoperaattori. 
# Esim. koska 51 = 7 * 7 + 2, 51 %% 7 = 2:
51%%7

#b)
v1 <- seq(8, 800, by = 8)

#c)
# Vihjeen mukaisesti sopivien alkioiden lukumaara voidaan laskea
# joko poimimalla ne omaan vektoriinsa ja laskemalla sen pituuden
pituus <- length(v1[v1%%7 == 0 | v1%%9 == 0])
# tai laskemalla sen rajaamiseen kaytetyn ehtolauseen palauttamassa
# totuusarvovektorissa esiintyvien TRUE-arvojen lukumaaran sum-funktiolla
# sum(v1%%7 == 0 | v1%%9 == 0)
