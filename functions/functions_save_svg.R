#' save a plot in svg format
#'
#' @param 
#' @param 
#' @param 
#' @param 
#'
#' @return NULL
#'
save_svg <- function(plot, print_plot = TRUE, file_name, width, height){
  
  if(isTRUE(print_plot)){
    print(plot)
  }
  
  svg(file_name, width = width, height = height)
  print(plot)
  dev.off()
  
  return(invisible(NULL))
}
