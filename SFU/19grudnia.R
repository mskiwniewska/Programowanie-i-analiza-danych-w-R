#5.36 
#5.42

#page source #pomiedzy <td>
#pkm
#wyswietl zrodlo strony

library(stringi)

data <- readLines("https://archive.org/download/stackexchange")


#stworz ramke danych (plik,data,wielkosc)

#posortowana po wielkosci malejaco (w bajtach)

data[530:542]


### stri_extract_all_regex(data,"^(<td>)(</td>)$")

###stri_extract_all_regex(data,"^(<td><a href=>)(</a>/td>)$")

library("xml2")
library("rvest")
data2 <- (read_html("https://archive.org/download/stackexchange") %>% html_table(fill=TRUE))[[1]]
head(data2)

data2 <- as.data.frame(data2)


#napisy czy czynniki

typeof(data2[,2])
#data2[,2] <- as.Date(data2[,2])
#strptime

#########

stri_extract_all_regex(data2[,3],"\\p{N}")



#######


g <- stri_detect_first_regex(data2,"<a href=.*7z")



