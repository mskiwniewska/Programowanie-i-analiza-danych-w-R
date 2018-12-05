### mapa US, 4.13, 4.10

#lotniska na mapie US

library(nycflights13)

head(airports[,c("faa","name","lat","lon")])

install.packages("maps")

library(maps)

map("usa")

points(as.matrix(airports[,c("lon","lat")]))


###Zad.4.13


t <- seq(0.2*pi,length.out=101)[-1]
x <- 16*sin(t)^3
y <- 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
plot.new()
plot.window()




#zad.4.10

osoby <- c("Janek", "Kasia", "Robert", "Jola", "Pszemek")
kto_kogo <- matrix(FALSE,
                   nrow=length(osoby),
                   ncol=length(osoby),
                   dimnames=list(osoby,osoby)
)
kto_kogo[cbind(
  c("Janek", "Robert", "Jola", "Pszemek", "Robert"),
  c("Kasia", "Kasia", "Kasia", "Jola",    "Pszemek")
)] <- TRUE
kto_kogo <- kto_kogo | t(kto_kogo) # symetria
kto_kogo



























