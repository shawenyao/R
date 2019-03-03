# PROBLEM SET 1 -----

# Use "which" and "letters" to find the position in the alphabet of the vowels
which(letters %in% c("a", "e", "i", "o", "u"))

# Generate the sequence 1^1, 2^2, 3^3, ..., 10^10
(1:10)^(1:10)

# Thanksgiving is the 4th Thursday of November.  Professor Rossi's birthay is Nov 25.  
# Find all years between (and including) 1950-2050 in which his birthday is on Thanksgiving. 
# (use "seq" and "as.Date")
library(magrittr)
paste0(1950:2050, "-11-25") %>% 
  as.Date() %>% 
  weekdays() %>% 
  equals("Thursday") %>% 
  sum()

