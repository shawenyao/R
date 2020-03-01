# js code to let the animation start on load
js_code <- paste0(
  "async function(el, x) {",
  "while(true){",
  "await new Promise(r => setTimeout(r, 1000));"
)

for(i in 1:(length(all_dates))){
  if(i != length(all_dates)){
    last_frame <- i - 1
    current_frame <- i
  }else{
    last_frame <- i - 1
    current_frame <- 0
  }
  
  js_code <- js_code %>% paste0(
    "await new Promise(r => setTimeout(r, 333));",
    "document.getElementsByTagName('input').item(", last_frame, ").click();",
    "document.getElementsByTagName('input').item(", current_frame, ").click();"
  )
}

js_code <- js_code %>% paste0("}}")
