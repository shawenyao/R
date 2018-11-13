#' extract the displate image url from given the page url
#' 
#' @param page_url the page url
#' 
#' @return the image url
#' 
get_img_url <- function(page_url){
  
  read_html(page_url) %>% 
    html_node(".gallery-container") %>% 
    xml_child(2) %>% 
    xml_attrs() %>% 
    `[[`("data-jetzoom") %>% 
    gsub(pattern = "zoomImage: ", replacement = "", .) %>% 
    gsub(pattern = "'", replacement = "", .)
}


#' download the image to designated loaction
#' 
#' @param page_url the page url
#' @param file_name the file name
#' 
#' @return NULL
#' 
download_img <- function(page_url, file_name){
  
  img_url <- get_img_url(page_url)
  
  download.file(url = img_url, destfile = paste0(file_name, ".jpg") %>% gsub(":", "", .), method = "curl", quiet = TRUE)
  
  return(invisible(NULL))
}


#' download all the images in a collection
#' 
#' @param collection_url the url of the collection
#' @param collection_name the name of the collection
#' @param total_page_number total number of pages in the collection
#' @param download_base_dir the base directory of download
#' 
#' @return NULL
#' 
download_collections <- function(collection_url, collection_name, total_page_number, download_base_dir){
  
  setwd(paste0(download_base_dir, collection_name))
  
  # loop over all the pages
  for(i in 1:21){
    
    page_html <- collection_url %>% 
      paste0("?page=", i) %>% 
      read_html() 
    
    page_urls <- page_html %>% 
      html_nodes(".displate-item-link") %>% 
      xml_attr("href")
    
    city_names <- page_html %>% 
      paste0(i) %>% 
      read_html() %>% 
      html_nodes(".collection-list-description") %>% 
      lapply(function(node){
        node %>% 
          xml_child(1) %>% 
          xml_child(1) %>% 
          html_text()
      }) %>% 
      unlist()
    
    # loop over all the images in each page
    for(j in seq_along(page_urls)){
      
      file_name <- paste0(i, "-", j, " ", city_names[j])
      print(file_name)
      
      download_img(page_url = page_urls[j], file_name = file_name)
    }
  }
  
  return(invisible(NULL))
}
