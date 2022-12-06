library(tidyverse)


d_raw <- read_lines("day_6/inputs_6.txt")

for(i in 1:nchar(d_raw)){
  chars <- str_sub(d_raw, i, i + 13)
  
  found <- str_detect(chars, "(.).*\\1", negate = T)
  
  if(found){
    break
  }

}

print(i)


str_sub(d_raw, i, i + 3)

i + 13
