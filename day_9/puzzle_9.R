library(tidyverse)

d <- read_delim("day_9/input_9.txt", delim = " ", col_names = c("dir", "dist"))

repeated <- d %>%
  rowwise() %>%
  mutate(dir_rep = list(rep(dir, dist)))

head_steps <- map2_dfr(1:nrow(repeated), repeated$dir_rep, ~expand_grid(step = .x, dir = .y))
# head_steps <- repeated$dir_rep %>% unlist()

head_pos_list <- list(tibble(#"step" = 0, 
  "x" = 0, "y" = 0))

for(i in 1:nrow(head_steps)){
  last_pos <- head_pos_list[[i]]
  
  cur_move <- head_steps[i,]
  
  if(cur_move$dir == "U"){
    next_pos <- last_pos %>% mutate(y = y + 1)
  }else if(cur_move$dir == "D"){
    next_pos <- last_pos %>% mutate(y = y - 1)
  }else if(cur_move$dir == "R"){
    next_pos <- last_pos %>% mutate(x = x + 1)
  }else if(cur_move$dir == "L"){
    next_pos <- last_pos %>% mutate(x = x - 1)
  }
  
  # next_pos$step <- i
  
  head_pos_list[[i + 1]] <- next_pos
}

head_pos_tbl <- bind_rows(head_pos_list)
head_pos_tbl$dir <- c(NA, head_steps$dir)

# summary(head_pos_tbl$y)


tail_pos_list <- list(tibble("x" = 0, "y" = 0))

for(i in 1:(nrow(head_pos_tbl) - 1)){
  head_pos <- head_pos_tbl[i + 1,]
  last_tail_pos <- tail_pos_list[[i]]
  
  x_diff <- head_pos$x - last_tail_pos$x
  y_diff <- head_pos$y - last_tail_pos$y
  
  if(abs(x_diff) > 1 | abs(y_diff) > 1){
    if(abs(x_diff) == 2 & y_diff == 0){
      tail_pos <- last_tail_pos %>%
        mutate(x = x + (.5 * x_diff))
    }else if(abs(y_diff) == 2 & x_diff == 0){
      tail_pos <- last_tail_pos %>%
        mutate(y = y + (.5 * y_diff))
    }else if(abs(y_diff) == 2 & abs(x_diff) == 1){ #diag move - y dist 2
      tail_pos <- last_tail_pos %>%
        mutate(x = x + x_diff,
               y = y + (.5 * y_diff))
    }else if(abs(x_diff) == 2 & abs(y_diff) == 1){ #diag move - x dist 2
      tail_pos <- last_tail_pos %>%
        mutate(x = x + (.5 * x_diff),
               y = y + y_diff)
    }
  }else(
    tail_pos <- last_tail_pos
  )
  
  tail_pos_list[[i + 1]] <- tail_pos #%>% mutate(step = i)
}

tail_pos_tbl <- bind_rows(tail_pos_list)

tail_pos_tbl %>%
  distinct() %>%
  nrow()

head_pos_tbl %>% 
  select(head_x = x, head_y = y) %>% 
  bind_cols(tail_pos_tbl) %>% View()

#### part 2


move_next_knot <- function(next_front_x, next_front_y, last_rear_x, last_rear_y){
  x_diff <- next_front_x - last_rear_x
  y_diff <- next_front_y - last_rear_y
  
  if(abs(x_diff) > 1 | abs(y_diff) > 1){
    if(abs(x_diff) == 2 & y_diff == 0){
      next_rear_x = last_rear_x + (.5 * x_diff)
      next_rear_y = last_rear_y
    }else if(abs(y_diff) == 2 & x_diff == 0){
      next_rear_y = last_rear_y + (.5 * y_diff)
      next_rear_x = last_rear_x
    }else if(abs(y_diff) == 2 & abs(x_diff) == 1){ #diag move - y dist 2
      next_rear_x = last_rear_x + x_diff
      next_rear_y = last_rear_y + (.5 * y_diff)
    }else if(abs(x_diff) == 2 & abs(y_diff) == 1){ #diag move - x dist 2
      next_rear_x = last_rear_x + (.5 * x_diff)
      next_rear_y = last_rear_y + y_diff
    }else if(abs(x_diff) == 2 & abs(y_diff) == 2){
      next_rear_x = last_rear_x + (.5 * x_diff)
      next_rear_y = last_rear_y + (.5 * y_diff)
    }
  }else{
    next_rear_x <- last_rear_x
    next_rear_y <- last_rear_y
  }
  
  return(tibble(x = next_rear_x, y = next_rear_y))
  
}

# move_next_knot(-1, -1, 0, 0)

num_knots <- 10

new_follow_knots <- expand_grid(x = 0, y = 0, knot = 2:num_knots, step = 0)

head_pos_tbl_2 <- head_pos_tbl %>%
  mutate(knot = 1) %>%
  select(-dir) %>%
  mutate(step = row_number() - 1)

knots_pos_tbl <- head_pos_tbl_2 %>% 
  bind_rows(new_follow_knots) %>%
  arrange(step, knot)

for(i in 1:max(knots_pos_tbl$step)){
# for(i in 1:5){
  if(i %% 100 == 0){
    print(i)
  }
  # print(i)
  # head_pos <- head_pos_tbl_2[i + 1,]
  
  
  for(j in 2:num_knots){
    # print(j)
    lead_pos <- knots_pos_tbl %>% filter(knot == (j - 1) & step == i)
    last_follow_pos <- knots_pos_tbl %>% filter(knot == j & step == i - 1)
    new_knot_pos <- move_next_knot(next_front_x = lead_pos$x, next_front_y = lead_pos$y, 
                                last_rear_x = last_follow_pos$x, last_rear_y = last_follow_pos$y) %>%
      mutate(knot = j, step = i)
    
    knots_pos_tbl <- knots_pos_tbl %>% bind_rows(new_knot_pos)
    
    
  }

}

knots_pos_tbl %>%
  arrange(step, knot) %>%
  View()


