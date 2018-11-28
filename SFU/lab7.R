#4.23 i 4.26

arrows()

set.seed(123)
x <- as.integer(c(rpois(15, 1), rpois(15, 2), rpois(15, 1.4), rpois(15, 4)))
g <- factor(rep(c("krasnale", "misie", "kwiatki", "œnie¿ki"), each = 15))


function(x,g) {
  
  par(mar=c(2.5, 6, 2, 2))
  #boxplot(iris[1:4], las=1, horizontal = TRUE)
 
  krasnale <- x[1:15]
  misie <- x[16:30]
  kwiatki <- x[31:45]
  œnie¿ki <- x[46:60]
  
  ramka <- data.frame(krasnale ,misie,kwiatki,œnie¿ki)
  
  boxplot(ramka[order(colMeans(ramka[1:4]))], las=1, horizontal = TRUE)
  
  
  
  print(par("usr"))
  points(colMeans(ramka[order(colMeans(ramka[1:4]))]), axTicks(2), col=2, pch=16) 
  
   
  
  
  
}


#data <- split(x,g)
#srednie <- sapply(data,mean)
#odchstd <- sapply(data,sd)
#o <- order(srednie)
#data <-data[o]
#srednie <- srednie[o]
#odchstd <- odchstd[o]
#boxplot(data,horizontal=TRUE,...,las=1)
#points(srednie,axTicks(2),pch=16,cex=2,col=4)
#arrows(srednie-d*odchstd,axTicks(2),srednie+d*odchstd,axTicks(2),code=3)
#lines(srednie,axTicks,col=2,lty=2)


#4.26


set.seed(1234)
g <- factor(c(rep("M", 15), rep("K", 15)))
h <- factor(sample(c("wysoki", "œredni", "niski"), replace = TRUE, 30))
table(g, h)
dane2 <- as.data.frame(table(g, h))


function(g,h,...){
  
  table(g, h)
  
  dane2 <- as.data.frame(table(g, h))
  
  
  barplot(dane2$Freq, las=1, xpd=FALSE)
  
  
  barplot(table(g, h), las=1, xpd=FALSE, beside=TRUE)
  
  
  
  
  #?grDevices ?graphics
  
}

























