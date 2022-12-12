library(tidyverse)

# remotes::install_github('machow/astar-r')
library(astar)


d_raw <- read_lines("day_12/input_12.txt")

d_raw %>%
  str_which("S")

d_raw %>% str_locate("E")



d_mat <- d_raw %>%
  map(~str_split(.x, "")) %>%
  map(unlist) %>%
  reduce(c) %>%
  matrix(nrow = length(d_raw), ncol = nchar(d_raw[1]), byrow = T)


start <- list("row" = 21, "col" = 1)
end <- list("row" = 21, "col" = 133)

which(d_mat == "S")

d_mat[21, 133]

estimate_cost <- function(node, goal){
  abs(node$row - goal$row) + abs(node$col - goal$col)
}

edge_distance <- function(node, neighbor){1}

neighbors <- function(node){
  node_letter <- d_mat[node$row, node$col]
  
  if(node_letter == "S"){
    node_height <- 1
  }else if(node_letter == "E"){
    node_height <- 26
  }else{
    node_height <- match(node_letter, letters)
  }
  
  up <- d_mat[node$row - 1, node$col]
  down <- d_mat[node$row + 1, node$col]
  left <- d_mat[node$row, node$col - 1 ]
  right <- d_mat[node$row, node$col + 1]
  
  height_list <- list(up, down, left, right) %>%
    map(~match(.x, letters))
  
  height_list_valid_lgl <- map(height_list, 
                                   function(x){node_height <= x + 1}) %>%
    map(function(x){if(length(x) == 0){FALSE}else{x}}) %>%
    unlist()
  
  neighbor_list <- list(list("row" = "")) 
  
  # height_list_valid_lgl <- map_lgl(height_list, function(x){length(x) > 0})
  
  # height_list_valid <- height_list[height_list_valid_lgl]
  
}