library(rio)
library(tidyverse)

dates <- seq(from = as.Date("2020-01-01"), to = as.Date(Sys.time()), by = 1)

bvol <- NULL

for(i in seq_len(length(dates) - 1)){
  
  print(dates[i])
  
  while(TRUE){
    
    new <- import(
      paste0(
        "https://www.bitmex.com/api/v1/trade?symbol=.BVOL&columns=price&startTime=",
        dates[i],
        "&endTime=",
        dates[i+1]
      ),
      format = "json"
    )
    
    if(!is.null(new$price)){
      
      bvol <- bvol %>% bind_rows(new)
      
      break
    }else{
      Sys.sleep(10)
    }
  }
}

export(
  bvol %>% mutate(timestamp = as.Date(timestamp)),
  "input/defi/bvol.csv"
)
