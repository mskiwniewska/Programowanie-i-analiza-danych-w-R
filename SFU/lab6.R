library("nycflights13")

flights <- as.data.frame(flights)
weather <- as.data.frame(weather)
airports <- as.data.frame(airports)
airlines <- as.data.frame(airlines)
planes <- as.data.frame(planes)

library(sqldf)

#zad.2.3.d

sqldf::sqldf("SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY year ASC, seats DESC")


a <- na.omit(unique(planes[planes$year >= 2012, c("year", "seats")]))

#order
b <- a[order(a$seats, decreasing =TRUE),] #najpierw po seats desc
c <- b[order(b$year, decreasing=FALSE),] #teraz po year asc

#zad.2.3.e

#analogicznie do zad.2.3.d

sqldf::sqldf("SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY seats DESC, year ASC")

x <- na.omit(unique(planes[planes$year >= 2012, c("year", "seats")]))

#order
y <- x[order(x$year, decreasing = FALSE),] #najpierw po year asc
z <- y[order(y$seats, decreasing= TRUE),] #teraz po seats desc

#Zad.2.4.a

sqldf::sqldf("SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer")

#robiê filtrowanie po wierszach
#stosujê to na kolumnie manufacturer
#tworzê table
#przekszta³cam do ramki danych

as.data.frame(table(planes$manufacturer[planes$seats>200]))


#Zad.2.4.b


sqldf::sqldf("SELECT manufacturer, COUNT(*) FROM planes GROUP BY manufacturer HAVING COUNT(*) > 10")

as.data.frame(table(planes$manufacturer)) -> odp
odp[odp$Freq>10,]

#zad.2.4.c


sqldf::sqldf("SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer HAVING COUNT(*) > 10")

as.data.frame(table(planes$manufacturer[planes$seats>200])) -> odp2
odp2[odp2$Freq>10,]


#Zad.2.5.a

sqldf::sqldf("SELECT * FROM flights LEFT JOIN planes ON flights.tailnum=planes.tailnum")
merge(flights,planes,by="tailnum",all.x=TRUE,all.y=FALSE) -> p

#Zad.2.5.b

sqldf::sqldf("SELECT planes.*, airlines.* FROM (SELECT DISTINCT carrier, tailnum FROM flights) AS cartail INNER JOIN planes ON cartail.tailnum=planes.tailnum INNER JOIN airlines ON cartail.carrier=airlines.carrier")









