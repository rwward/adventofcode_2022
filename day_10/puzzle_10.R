library(tidyverse)

d_raw <- read_lines("day_10/input_10.txt")

d <- d_raw %>%
  tibble("instruction" = .) %>%
  separate(instruction, c("instruction", "amount"), sep = " ") %>%
  mutate(amount = as.numeric(amount))


register <- 1L

cycle <- 1

val_list <- list(tibble("cycle" = 1, "register" = 1))

for(i in 1:nrow(d)){
  if(d$instruction[i] == "noop"){
    
    
    out <- tibble("cycle" = cycle + 1, "register" = register)
    
    cycle <- cycle + 1
    
  }else{
     out <- tibble("cycle" = c(cycle + 1, cycle + 2), "register" = c(register, register + d$amount[i]))
     
     cycle <- cycle + 2
     register <- register + d$amount[i]
  }
  
  val_list[[length(val_list) + 1]] <- out
}


val_tbl <- val_list %>% bind_rows()


val_tbl %>%
  mutate(signal_strength = cycle * register) %>%
  slice(seq(20, 220, by = 40)) %>%
  pull(signal_strength) %>%
  sum()

drawn <- val_tbl %>%
  mutate(screen_col = (cycle - 1) %% 40) %>%
  rowwise() %>%
  mutate(sprite_cols = list(c((register-1):(register+1)))) %>%
  mutate(draw = screen_col %in% sprite_cols)

pixels <- drawn$draw[1:240] %>%
  as.character() %>%
  str_replace_all("TRUE", "#") %>%
  str_replace_all("FALSE", ".") %>%
  matrix(nrow = 6, ncol = 40, byrow = T) 
  

rows <- pixels %>%
  apply(MARGIN = 1, FUN = function(x){str_c(x, collapse = "")})

rows

screen <- matrix(nrow = 6, ncol = 40, byrow = )

for(i in 1:nrow(val_tbl)){
  cur_cy <- val_tbl$cycle[i]
  
  cur_reg <- val_tbl$register[i]
  
  screen_row <- cur_cy %/% 40
}
