library(tidyverse)
library(parallel)
library(beepr)

#' find the longest run of heads given a certain number of binary experiments
#' 
#' @param size the number of binary draws
#' 
#' @return the longest run of heads in binary draws of size equals to `size`
#' 
find_longest_run_of_heads <- function(iteration, size){
  
  sample(
    x = c(0, 1),
    size = size,
    replace = TRUE
  ) %>% 
    paste0(collapse = "") %>% 
    strsplit("0") %>% 
    `[[`(1) %>% 
    nchar() %>% 
    max()
}


#===== general setup =====
set.seed(1)
n <- 1000000
size <- 100


#===== start experiment =====
start_time <- proc.time()

cl <- makeCluster(spec = detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
results <- parLapply(
  cl, 
  seq_len(n), 
  find_longest_run_of_heads, 
  size = size
) %>% 
  unlist()

stopCluster(cl)

proc.time() - start_time

mean(results)


#===== plot ======
tibble(longest_run_of_heads = results) %>% 
  group_by(longest_run_of_heads) %>% 
  summarise(count = n() / n) %>% 
  ggplot(aes(x = longest_run_of_heads)) +
  geom_bar(aes(weight = count)) +
  xlab("Longest Run of Heads") +
  ylab("Count")

# play sound when finished
beep(sound = 2)
