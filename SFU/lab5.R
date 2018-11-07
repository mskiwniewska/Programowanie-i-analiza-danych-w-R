install.packages("nycflights13")
library("nycflights13")

flights <- as.data.frame(flights)
weather <- as.data.frame(weather)
airports <- as.data.frame(airports)
airlines <- as.data.frame(airlines)
planes <- as.data.frame(planes)

#nrow ncol tail dim head
dim(flights)
?flights
head(flights)

View(flights)
View(weather)
View(airports)
View(airlines)
View(planes)


install.packages("sqldf")

##a
sqldf::sqldf("SELECT DISTINCT engine FROM planes")

unique(planes$engine)

data.frame(engine=unique(planes$engine))

##b
sqldf::sqldf("SELECT DISTINCT type, manufacturer FROM planes")

planes[,3:4]

unique(planes[,3:4])

data.frame(unique(planes[,3:4]))

nrow(data.frame(unique(planes[,3:4])))

names(data.frame(unique(planes[,3:4])))

##c

sqldf::sqldf("SELECT COUNT(*), engine FROM planes GROUP BY engine")

split(planes$tailnum,planes$engine)
lapply(split(planes$tailnum,planes$engine),length)

data.frame(unlist(lapply(split(planes$tailnum,planes$engine),length)))

names(data.frame(unlist(lapply(split(planes$tailnum,planes$engine),length))))

#split,aggregate

"SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type"


"SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer FROM planes GROUP BY engine, manufacturer"



###Zad.2.3

###a
sqldf::sqldf("SELECT * FROM planes WHERE speed IS NOT NULL")

as.numeric(!is.na(planes$speed))
which(as.numeric(!is.na(planes$speed))==1)

planes[which(as.numeric(!is.na(planes$speed))==1),]


sqldf::sqldf("SELECT tailnum FROM planes WHERE seats BETWEEN 150 AND 190 AND year >= 2012")



sqldf::sqldf('SELECT * FROM planes WHERE manufacturer IN ("BOEING", "AIRBUS", "EMBRAER") AND seats>390')
sqldf::sqldf("SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY year ASC, seats DESC")
sqldf::sqldf("SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY seats DESC, year ASC")



