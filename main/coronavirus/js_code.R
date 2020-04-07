# js code to let the animation start on load and loop for certain times
js_code <- paste0(
  "async function(el, x) {",
  "await new Promise(r => setTimeout(r, 1000));",
  "count = 0;",
  "while(true){"
)

for(i in 1:(length(all_dates))){
  
  if(i != length(all_dates)){
    current_frame <- length(all_dates) - i - 1
  }else{
    current_frame <- length(all_dates) - 1
    
    # additional wait on the last frame
    js_code <- js_code %>% paste0(
      "await new Promise(r => setTimeout(r, 1500));",
      "if(count == 1 - 1) { break; }"
    )
  }
  
  js_code <- js_code %>% paste0(
    # controls frames per second
    "await new Promise(r => setTimeout(r, 100));",
    "document.getElementsByTagName('input').item(", current_frame, ").click();"
  )
}

js_code <- js_code %>% paste0(
  "count = count + 1;",
  "}}"
)
