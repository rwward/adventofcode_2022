library(tidyverse)

stacks_raw <- read_lines("day_5/input_5_1.txt") %>%
  head(8)

stacks_proc <- stacks_raw %>% 
  str_replace_all("^   ", "[X]") %>%
  str_replace_all("    ", " [X]") %>%
  str_split(pattern = " ") %>%
  map(~str_replace_all(.x, "\\[", "")) %>%
  map(~str_replace_all(.x, "\\]", "")) %>%
  unlist() %>%
  matrix(ncol = 9, byrow = T) %>%
  as_tibble() %>%
  na_if("X")


names(stacks_proc) <- str_c("stack_", 1:9)

stacks_list <- stacks_proc %>% 
  as.list() %>%
  map(~.x[!is.na(.x)]) %>%
  map(rev)

moves <- read_delim("day_5/input_5_2.txt", delim = " ", 
                        col_select = c(2, 4, 6),
                        col_names = c("a", "num_moved", "b", "source", "c", "dest"))


move_containers <- function(stacks, crates_moved, source_stack, dest_stack){
  
  for(i in 1:crates_moved){
    crate_to_move <- pluck(stacks, source_stack, length(stacks[[source_stack]]))
    
    stacks[[dest_stack]][length(stacks[[dest_stack]]) + 1] <- crate_to_move
    
    stacks[[source_stack]] <- stacks[[source_stack]][1:(length(stacks[[source_stack]]) - 1)]
  }
  
  return(stacks)
  
}

# stacks_step_2 <- move_containers(stacks_list, 1, 5, 6)

# move_containers(stacks_step_2, 5, 6, 7)

stacks_out <- stacks_list

for(i in 1:nrow(moves)){
  stacks_out <- move_containers(stacks = stacks_out, 
                                crates_moved = moves$num_moved[i], 
                                source_stack = moves$source[i],
                                dest_stack = moves$dest[i])
}


move_containers_9001 <- function(stacks, crates_moved, source_stack, dest_stack){
  
  
    source_height <- length(stacks[[source_stack]])
  
    crates_to_move <- stacks[[source_stack]][(source_height - crates_moved + 1):source_height]
    
    dest_height <- length(stacks[[dest_stack]])
    
    stacks[[dest_stack]][(dest_height + 1):(dest_height + crates_moved)] <- crates_to_move
    
    stacks[[source_stack]] <- stacks[[source_stack]][1:(source_height - crates_moved)]
  # }
  
  return(stacks)
  
}

stacks_out_9001 <- stacks_list

for(i in 1:nrow(moves)){
  stacks_out_9001 <- move_containers_9001(stacks = stacks_out_9001, 
                                crates_moved = moves$num_moved[i], 
                                source_stack = moves$source[i],
                                dest_stack = moves$dest[i])
}



stacks_out_9001
