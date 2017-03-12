#test 1 (20min)
findNum <- function(y,l){
  x <- vector(mode = "numeric", length = 0)
  j=0
  for(i in 1 : (length(y)-1)){
    if((y[i] == l) && (y[i+1] == l)){
      j = j+1
      x[j] <- i
    }
  }
  print(x)
}


#test2 (30min)

raw <- read.delim("C:\\Advanced R\\weather.txt",check.names = F, na.strings = ".")
raw = as.matrix(raw)
data1 = t(raw)
#creat a new column to store the temperature difference
A <- matrix(nrow = 34,ncol = 1)
data2 = cbind(data1, A)
#character to numeric
data3 = matrix(as.numeric(data2),nrow =nrow(data2))
#computing
for(i in 4:34){
  a = max(na.omit(data3[i,]))
  b = min(na.omit(data3[i,]))           
  data3[i,23] = a - b
}
TempDiff = rbind(data2[1:3, ], data3[4:34, ])



#test3 (60min)
library(hflights)
str(hflights)

library(dplyr)
packageVersion("dplyr")
tbl_hflights<-tbl_df(hflights)
class(tbl_hflights)
tbl_hflights

# variables selection
tbl_hflights1 <- select(tbl_hflights, Month, UniqueCarrier,ArrDelay)
uniquecarrier <- unique(tbl_hflights$UniqueCarrier)
month <- unique(tbl_hflights$Month)
#computing the  quantile and storing in the matrix
A = matrix(0,15,12)
colnames(A) <- month
rownames(A) <- uniquecarrier
for(i in 1:length(uniquecarrier)){
  for(j in 1:12){
    tbl_hflights2<-filter(tbl_hflights1,Month == j & UniqueCarrier == uniquecarrier[i])
    x = quantile(tbl_hflights2$ArrDelay,probs = 0.1, na.rm = TRUE)
    A[i, j] = x
  }
}
#mean
rowMeans(A,na.rm = TRUE)

