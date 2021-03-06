js_code <- paste0(
  
  "async function(el, x) {",
  "total_frames = ", length(all_dates), ";",
  
  "await new Promise(r => setTimeout(r, 1500));",
  
  "var i;",
  "for (i = 0; i < total_frames; i++) {",
  "current_frame = total_frames - i - 1;",
  
  # controls frames per second
  "await new Promise(r => setTimeout(r, 50));",
  
  "document.getElementsByTagName('input').item(current_frame).click();",
  "}",
  
  "}"
)