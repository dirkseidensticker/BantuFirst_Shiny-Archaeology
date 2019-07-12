# c14bazAAR for aDRAC

library(c14bazAAR)
library(dplyr)
library(tidyr)

d <- get_aDRAC() %>% 
  calibrate(choices = c("calrange"), sigma = 2)


for(i in 1:nrow(d)){
  li <- d[i,"calrange"][[1]]
  d[i,"TO"] <- 1950 - min(li[[1]]$from, na.rm = T)
  d[i,"FROM"] <- 1950 - max(li[[1]]$from, na.rm = T)
}

d$calrange <- NULL

write.csv(d, "c14bazAAR_aDRAC.csv")
