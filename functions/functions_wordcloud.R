
#' count the frequency of word
#' 
#' @param book a vector of sentences
#' 
#' @return a data.frame of word frequencies
#' 
summarise_word_count <- function(book){
  
  tibble(text = book) %>% 
    # break the sentence into words
    unnest_tokens(word, text) %>% 
    # exlucde stopwords
    anti_join(get_stopwords(), by = "word") %>% 
    # count the frequency
    count(word, sort = TRUE)
}


#' label the words that are exclusive to one particular book
#' 
#' @param i iterator
#' @param word_count_list a list of data.frame of word counts
#' 
#' @return a data.frame with the exlusive label
#' 
label_exclusive_words <- function(i, word_count_list){
  
  word_count_list[[i]] %>% 
    select(-n) %>% 
    # exclude the words that also appear in other books
    anti_join(word_count_list[setdiff(seq_along(word_count_list), i)] %>% bind_rows(), by = c("word")) %>% 
    # set exlusive indicator
    mutate(exclusive = TRUE) %>% 
    # bring back the full list of words
    right_join(word_count_list[[i]], by = c("word")) %>% 
    # label the exlusive words accordingly
    mutate(
      exclusive = if_else(is.na(exclusive), FALSE, TRUE),
      book = i
    )
}
