# js code to let the animation start on load and loop for certain times
js_code <- paste0(
  "async function(el, x) {",
  "await new Promise(r => setTimeout(r, 1000));",
  "count = 0;",
  "while(true){"
)

for(i in 1:(length(all_dates))){
  
  last_frame <- i - 1
  
  if(i != length(all_dates)){
    current_frame <- i
  }else{
    current_frame <- 0
    
    # additional wait on the last frame
    js_code <- js_code %>% paste0(
      "await new Promise(r => setTimeout(r, 2000));",
      "if(count == 10) { break; }"
    )
  }
  
  js_code <- js_code %>% paste0(
    "await new Promise(r => setTimeout(r, 250));",
    "document.getElementsByTagName('input').item(", last_frame, ").click();",
    "document.getElementsByTagName('input').item(", current_frame, ").click();"
  )
}

js_code <- js_code %>% paste0(
  "count = count + 1;",
  "}}"
)
