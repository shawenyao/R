#' parse argument name/value pairs passed from batch call
#' and assign them accordingly in the global environment
#'
#' @param print_on switch to turn on printing parsed name/value pairs to console
#' 
#' @description 
#' "-arg1 value1 -arg2 value2 -arg3 value3"
#' will be equivalent to
#' arg1 <- "value1"
#' arg2 <- "value2"
#' arg3 <- "value3"
#' 
#' @return (invisible) the names of the assigned arguments
#' 
parse_args <- function(print_on = TRUE){
  
  # a string of name/value pairs
  # e.g., "-arg1 value1 -arg2 value2 -arg3 value3"
  args <- commandArgs(trailingOnly = TRUE)
  
  # odd elements are the names of the arguments (without the leading "-")
  args_names <- gsub("-", "", args[c(TRUE, FALSE)])
  # even elements are the values of the arguments
  args_values <- args[c(FALSE, TRUE)]
  
  # loop over the arguments to assign values
  for(i in seq_along(args_names)){
    assign(x = args_names[i], value = args_values[i], envir = .GlobalEnv)
    
    if(isTRUE(print_on)){
      print(
        paste0(args_names[i], " = ", get(x = args_names[i], envir = .GlobalEnv))
      )
    }
  }
  
  invisible(args_names)
}


#==== detect whether R script is launched in batch mode or RStudio mode ====
batch_mode_on <- is.na(Sys.getenv("RSTUDIO", unset = NA))


#==== assign args values accordingly ====
if(batch_mode_on){
  cat("Running in batch mode\r\n")
  
  # assign args passed from batch call
  parse_args(print_on = FALSE)  
  
}else{
  cat("Running in RStudio mode\r\n")
  
  # assign args manually
  arg1 <- "m1"
  arg2 <- "m2"
  arg3 <- "m3"
}


#==== validate args values ====
print(paste0("arg1 = ", arg1))
print(paste0("arg2 = ", arg2))
print(paste0("arg3 = ", arg3))


#==== prevent batch mode from closing itself ====
if(batch_mode_on){
  cat("Job finished. Press CTRL + C to exit.\r\n")
  invisible(readLines(file("stdin"), 1))
}
