library(tidyverse)

d_raw <- read_lines("day_8/input_8.txt")

mat <- d_raw %>%
  str_split("") %>%
  unlist() %>%
  map(as.numeric) %>%
  matrix(nrow = length(d_raw), ncol = nchar(d_raw[1]), byrow = T) #%>%
  # as.data.frame()
  

# mat <- as.data.frame(mat)

i <- 5
j <- 5

width <- ncol(mat)
height <- nrow(mat)

vis_list <- list()

for(i in 2:(height - 1)){
  for(j in 2:(width - 1)){

# for(i in 2:5){
  # for(j in 2:5){
    
    cur_tree_height <- mat[i, j]
    # left <- mat[i, 1:(j - 1)] %>% unlist()
    # 
    # vis_left <- mat[i,j] > max(left)
    
    vis_left <- cur_tree_height > (mat[i, 1:(j - 1)] %>% unlist() %>% max())
    
    vis_right <- cur_tree_height > (mat[i, (j + 1):width] %>% unlist() %>% max())
    
    vis_top <- cur_tree_height > (mat[1:(i - 1), j] %>% unlist() %>% max())
    
    vis_bottom <- cur_tree_height > (mat[(i + 1):height, j] %>% unlist() %>% max())
    
    vis_list[[length(vis_list) + 1]] <- tibble("row" = i,
                                               "col" = j,
                                               "vis_left" = vis_left,
                                               "vis_right" = vis_right,
                                               "vis_top" = vis_top,
                                               "vis_bottom" = vis_bottom)
    
  }
}

vis_tbl <- vis_list %>% 
  bind_rows() %>%
  mutate(vis = (vis_left|vis_right|vis_top|vis_bottom))


sum(vis_tbl$vis) + 99 + 99 + 97 + 97


### part 2

# vis_tbl %>%
#   mutate(vis_total = vis_left + vis_right + vis_top + vis_bottom) %>%
#   filter(vis) %>%
#   View()

vis_dist_list <- list()

# i <- 18
# j <- 61

# i <- 5
# j <- 5

get_vis_dist <- function(view, cur_height){
  dist <- purrr::detect_index(view, function(x) x >= cur_height)
  
  if(dist == 0){
    dist <- length(view)
  }
  
  return(dist)
  
}

for(i in 2:(height - 1)){
  for(j in 2:(width - 1)){
    
    # for(i in 2:5){
    # for(j in 2:5){
    
    cur_tree_height <- mat[i, j] %>% unlist()

    left <- mat[i, 1:(j - 1)] %>% unlist()
    
    right <- mat[i, (j + 1):width] %>% unlist()
    
    # purrr::detect_index(right, function(x) x >= cur_tree_height)
    
    top <- mat[1:(i - 1), j] %>% unlist()
    
    bottom <- mat[(i + 1):height, j] %>% unlist()
    
    dist_row <- list("left" = rev(left), "right" = right, "top" = rev(top), bottom = bottom) %>%
      map(~get_vis_dist(view = .x, cur_height = cur_tree_height)) %>%
      bind_cols() %>%
      mutate(row = i,
             col = j)
    
    
    vis_dist_list[[length(vis_dist_list) + 1]] <- dist_row
    
  }
}


vis_dist_list %>%
  bind_rows() %>%
  mutate(scenic_score = left * right * top * bottom) %>%
  arrange(desc(scenic_score))
