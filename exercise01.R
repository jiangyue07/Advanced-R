library(dplyr)
ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))
#Times(names)函数：传入产品名称，返回重叠时间范围的最大次数以及对应的时间范围
Times <- function(name){
  data1 <- mutate(ds,s = as.numeric(substr(ds$start, 1, 2)) + as.numeric(substr(ds$start, 4, 5))/60,
                  e = as.numeric(substr(ds$end, 1, 2)) + as.numeric(substr(ds$end, 4, 5))/60) 
  data_anest <- filter(data1, anest == name)
  result <- NULL
  m = length(data_anest$id)
  for(n in m:1){
    for(i in 1:choose(m , n)){
      x <- c(1:m)
      A <- combn(x,n)
      data_n <- slice(data_anest, A[,i])
      if(max(data_n$s) < min(data_n$e)){
        result <- list(n, data_n$id)
        print(result)
      }
    }
    if(is.null(result) == FALSE)
      break
  }
}
Times("baker")
Times("dow")