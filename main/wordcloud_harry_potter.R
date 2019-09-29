suppressWarnings(library(tidyverse))
suppressWarnings(library(tidytext))
suppressWarnings(library(wordcloud))
suppressWarnings(library(beepr))

set.seed(1)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/R")
source("functions/functions_wordcloud.R", echo=TRUE)

input_dir <- "input/Harry Potter"
output_dir <- "output/wordcloud"

color_palette <- tibble(
  book = 1:7,
  exclusive = TRUE,
  color = c(
    rgb(135, 37, 36, maxColorValue = 255),
    rgb(38, 35, 82, maxColorValue = 255),
    rgb(109, 38, 130, maxColorValue = 255),
    rgb(247, 43, 42, maxColorValue = 255),
    rgb(254, 218, 59, maxColorValue = 255),
    rgb(1, 82, 161, maxColorValue = 255),
    rgb(93, 65, 106, maxColorValue = 255)
  )
)


#==== input ====
book_list <- list.files(input_dir, pattern = "*.txt") %>% 
  paste0(input_dir, "/", .) %>% 
  map(read_lines)


#==== word count ====
# find the word count for every book
word_count_list <- book_list %>% 
  map(summarise_word_count)

# find the top words for every book
word_count_top_list <- word_count_list %>% 
  map(function(df){df %>% slice(1:200)})

# find the exclusive words for every book
word_count_top_exclusive_list <- seq_along(word_count_top_list) %>% 
  map(label_exclusive_words, word_count_list = word_count_top_list) %>% 
  # label the exclusive words and assign color
  map(function(df){
    df %>% 
      left_join(color_palette, by = c("book", "exclusive")) %>% 
      mutate(color = if_else(is.na(color), "grey", color))
  })


#==== output ====
# loop over the books
for(i in seq_along(word_count_top_exclusive_list)){
  
  png(paste0(output_dir, "/", i, ".png"), width = 880, height = 880, type = "cairo")
  word_count_top_exclusive_list[[i]] %>%
    with(wordcloud(
      words = word,
      freq = n, 
      scale = c(7.5, 2), 
      colors = color, 
      ordered.colors= TRUE,
      random.order = TRUE,
      rot.per = 0.2
    ))
  dev.off()
}

# play sound when finished
beep(sound = 2)
