library(tidyverse)
# library(Brobdingnag)
# library(Rmpfr)
library(gmp)

divisible_by <- function(num, divisor){
  return(num %% divisor == 0)
}

d_raw <- read_lines("day_11/input_11.txt")

monkey_list <- list()

for(i in 1:length(d_raw)){
  
  cycle <- i %% 7
  
  cur_line <- d_raw[i]
  
    if(cycle == 1){
      monkey_list[length(monkey_list) + 1] <- c("Monkey number" = as.numeric(
        str_extract(cur_line, "\\d")))
    }
}

monkey_list <- list()

start_ind <- seq(1, 50, by = 7)

for(i in 1:8){
  monkey_list[[i]] <- d_raw[start_ind[i]:(start_ind[i] + 5)]
}

# monkey_list[[1]]

# monkey_list[[1]][2] %>% str_extract_all("\\d\\d") %>% unlist()

# tibble("monkey" = monkey_list[[1]][1] %>% parse_number(),
#        "items" = monkey_list[[1]][2] %>% str_extract_all("\\d\\d"),
#        "operation" = monkey_list[[1]][3] %>% str_replace("Operation:", "") %>% str_squish(),
#        "test" = monkey_list[[1]][4] %>% parse_number(),
#        "throw_if_true" = monkey_list[[1]][5] %>% parse_number(),
#        "throw_if_false" = monkey_list[[1]][5] %>% parse_number()) 


parse_monkey <- function(monkey_list_entry){
  tibble("monkey" = monkey_list_entry[1] %>% 
           parse_number(),
         "items" = monkey_list_entry[2] %>% 
           str_extract_all("\\d\\d") %>% 
           map(as.numeric),
         "operation" = monkey_list_entry[3] %>% 
           str_replace("Operation: new = ", "") %>%
           str_replace_all("old", "worry_start") %>%
           str_squish(),
         "test" = monkey_list_entry[4] %>% 
           parse_number(),
         "throw_if_true" = monkey_list_entry[5] %>% 
           parse_number(),
         "throw_if_false" = monkey_list_entry[6] %>% 
           parse_number()) 
}

monkey_tbl <- monkey_list %>% map_dfr(parse_monkey) %>%
  mutate(inspection_counter = 0)
monkey_tbl_start <- monkey_tbl

# rm(i); rm(j)
# i <- 1
# j <- 1

for(r in 1:20){
# for(r in 1:1){
  print(str_c("Round ", r))
  #one round
  for(i in 1:nrow(monkey_tbl)){ #each monkey
    print(str_c("Monkey: ", i - 1))
    
    # for(j in (1:length(monkey_tbl[i,]$items[[1]]))){#each item held by that monkey
    #while that monkey has items
    while(length(monkey_tbl[i,]$items[[1]]) > 0){
      # print(str_c("Monkey has ", length(monkey_tbl[i,]$items[[1]]), " items remaining."))
      monkey_tbl[i,]$inspection_counter <- monkey_tbl[i,]$inspection_counter + 1
      
      cur_monkey_row <- monkey_tbl[i,]
      # print(cur_monkey_row)
      
      worry_start <- cur_monkey_row$items[[1]][1]
      # print(str_c("inspecting item: ", worry_start))
      
      # print(str_c("item: ", j, " with worry level: ", worry_start))
      
      worry_postop <- eval(parse(text = cur_monkey_row$operation))
      print(worry_postop)
      
      worry_bored <- floor(worry_postop / 3)
      
      test_passed <- divisible_by(worry_bored, cur_monkey_row$test)
      
      
      if(test_passed){
        print(str_c("Test passed, throwing to Monkey ", cur_monkey_row$throw_if_true))
        monkey_tbl[cur_monkey_row$throw_if_true + 1,]$items[[1]] <- 
          monkey_tbl[cur_monkey_row$throw_if_true + 1,]$items[[1]] %>% 
          append(worry_bored)
        
      }else{
        print(str_c("Test failed, throwing to Monkey ", cur_monkey_row$throw_if_false))
        monkey_tbl[cur_monkey_row$throw_if_false + 1,]$items[[1]] <- 
          monkey_tbl[cur_monkey_row$throw_if_false + 1,]$items[[1]] %>% 
          append(worry_bored)
      }
      
      #remove thrown item from throwing monkey's list
      if(length(cur_monkey_row$items[[1]]) > 1){
        monkey_tbl[i,]$items[[1]] <- cur_monkey_row$items[[1]][2:length(cur_monkey_row$items[[1]])]
      }else{
        monkey_tbl[i,]$items[[1]] <- numeric()
      }
      
    }
  }
}



monkey_tbl

top_two_active <- monkey_tbl %>%
  arrange(desc(inspection_counter)) %>%
  head(2) %>%
  pull(inspection_counter)

top_two_active[1] * top_two_active[2]

# cur_monkey_row <- monkey_tbl[i,]
# 
# worry_start <- cur_monkey_row$items[[1]][j]
# 
# worry_postop <- eval(parse(text = cur_monkey_row$operation[j]))
# 
# worry_bored <- floor(worry_postop / 3)
# 
# test_passed <- divisible_by(worry_bored, cur_monkey_row$test[j])
# 
# 
# 
# if(test_passed){
# 
#   monkey_tbl[cur_monkey_row$throw_if_true,]$items[[1]] <- 
#     monkey_tbl[cur_monkey_row$throw_if_true,]$items[[1]] %>% 
#     append(worry_bored)
#   
# }else{
#   monkey_tbl[cur_monkey_row$throw_if_false,]$items[[1]] <- 
#     monkey_tbl[cur_monkey_row$throw_if_false,]$items[[1]] %>% 
#     append(worry_bored)
# }
# 
# monkey_tbl[i,]$items[[1]] <- cur_monkey_row$items[[1]][2:length(cur_monkey_row$items[[1]])]


#### part 2

monkey_tbl <- monkey_list %>% 
  map_dfr(parse_monkey) %>%
  mutate(inspection_counter = 0) %>%
  mutate(items = items %>% map(as.character)) %>%
  mutate(items = items %>% map(as.bigz))
  # mutate(items = items %>% map(~mpfr(.x, 512)))

# monkey_tbl$items[[1]][5]

monkey_tbl_start <- monkey_tbl

lcm <- reduce(monkey_tbl$test, lcm.bigz)



# tictoc::tic()
for(r in 1:10000){
  # for(r in 1:1){
  
  if(r %% 50 == 0){
    # tictoc::toc()
    print(str_c("Round ", r))
    # tictoc::tic()
  }
  
  #one round
  for(i in 1:nrow(monkey_tbl)){ #each monkey
    # print(str_c("Monkey: ", i - 1))
    
    # for(j in (1:length(monkey_tbl[i,]$items[[1]]))){#each item held by that monkey
    #while that monkey has items
    while(length(monkey_tbl[i,]$items[[1]]) > 0){
      # print(str_c("Monkey has ", length(monkey_tbl[i,]$items[[1]]), " items remaining."))
      monkey_tbl[i,]$inspection_counter <- monkey_tbl[i,]$inspection_counter + 1
      
      cur_monkey_row <- monkey_tbl[i,]
      # print(cur_monkey_row)
      
      worry_start <- cur_monkey_row$items[[1]][1]
      # print(str_c("inspecting item: ", worry_start))
      
      # print(str_c("item: ", j, " with worry level: ", worry_start))
      
      worry_postop <- eval(parse(text = cur_monkey_row$operation)) %% lcm
      # print(worry_postop)
      
      # worry_bored <- floor(worry_postop / 3)
      
      test_passed <- divisible_by(eval(parse(text = cur_monkey_row$operation)), cur_monkey_row$test)
      
      
      if(test_passed){
        # print(str_c("Test passed, throwing to Monkey ", cur_monkey_row$throw_if_true))
        monkey_tbl[cur_monkey_row$throw_if_true + 1,]$items[[1]] <- 
          monkey_tbl[cur_monkey_row$throw_if_true + 1,]$items[[1]] %>% 
          append(worry_postop)
        
      }else{
        # print(str_c("Test failed, throwing to Monkey ", cur_monkey_row$throw_if_false))
        monkey_tbl[cur_monkey_row$throw_if_false + 1,]$items[[1]] <- 
          monkey_tbl[cur_monkey_row$throw_if_false + 1,]$items[[1]] %>% 
          append(worry_postop)
      }
      
      #remove thrown item from throwing monkey's list
      if(length(cur_monkey_row$items[[1]]) > 1){
        monkey_tbl[i,]$items[[1]] <- cur_monkey_row$items[[1]][2:length(cur_monkey_row$items[[1]])]
      }else{
        monkey_tbl[i,]$items[[1]] <- as.bigz(numeric())
      }
      
    }
  }
}

monkey_tbl
monkey_tbl$items %>% map(length) %>% unlist() %>% sum()

top_two_active <- monkey_tbl %>%
  arrange(desc(inspection_counter)) %>%
  head(2) %>%
  pull(inspection_counter)

top_two_active[1] * top_two_active[2]

# monkey_tbl$items



# googol <- mpfr(10, 256) ^ 100
# 
# googol %% 7

