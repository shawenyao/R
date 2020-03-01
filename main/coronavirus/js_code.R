# js code to let the animation start on load
js_code <- "async function(el, x) {" %>% 
  paste0("await new Promise(r => setTimeout(r, 1000));")

for(i in 1:(length(all_dates) - 1)){
  js_code <- js_code %>% paste0(
    "await new Promise(r => setTimeout(r, 333));",
    "document.getElementsByTagName('input').item(", i-1, ").click();",
    "document.getElementsByTagName('input').item(", i, ").click();"
  )
}

js_code <- js_code %>% paste0("}")
