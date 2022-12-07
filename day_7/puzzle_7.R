library(tidyverse)

d_raw <- read_lines("day_7/input_7.txt")


d <- tibble(term = d_raw) %>%
  mutate(wd = "/")


for(i in 2:nrow(d)){
  if(str_detect(d$term[i], fixed("$ cd"))){
    if(str_detect(d$term[i], fixed(".."))){
      d$wd[i] <- str_extract(d$wd[i - 1], "^(.*/)") %>% str_sub(1, -2)
    }else{
      d$wd[i] <- str_c(d$wd[i - 1], "/", str_extract(d$term[i], "\\b(\\w+)$"))
    }
  }else{
    d$wd[i] <- d$wd[i - 1]
  }
  
}

d_wds <- d %>%
  mutate(wd = str_replace(wd, fixed("//"), fixed("/"))) %>%
  mutate(dir = str_extract(wd, "\\w+$")) %>%
  mutate(dir = ifelse(is.na(dir), "/", dir)) %>%
  mutate(change_dir_shown = str_detect(term, "cd")) %>%
  mutate(dir_split = str_split(wd, "/")) %>% 
  rowwise() %>%
  mutate(parent_dir = dir_split[length(dir_split) - 1]) #%>%
  # mutate(double_parent_dir = dir_split[length(dir_split) - 2])


files <- d_wds %>%
  filter(str_detect(term, "\\d")) %>%
  separate(col = term, into = c("size", "filename"), sep = " ") %>%
  mutate(size = as.numeric(size))



all_dirs <- d_wds %>%
  select(parent_dir, dir) %>%
  distinct() %>%
  mutate(dir_with_parent = str_c(parent_dir, "/", dir)) %>%
  mutate(dir_with_parent = str_replace(dir_with_parent, fixed("//"), fixed("/")))

# all_dirs %>% filter(dir == "fdcdh")

# repeat_dirs <- all_dirs %>%
#   group_by(dir) %>%
#   add_count() %>%
#   filter(n > 1) %>%
#   arrange(dir)
# 
# 
# d_wds %>%
#   filter(dir %in% repeat_dirs$dir) %>%
#   select(parent_dir, dir, wd) %>%
#   distinct() %>%
#   arrange(dir) %>%
#   group_by(parent_dir, dir) %>%
#   add_count() %>%
#   arrange(desc(n))


dir_size <- function(dir_name, file_tbl){
  dir_size_out <- file_tbl %>%
    filter(str_detect(wd, dir_name)) %>%
    # group_by(parent_dir, dir) %>%
    summarize(total_size = sum(size)) %>%
    pull(total_size)
  
  # return(dir_size_out)
  
  return(tibble(dir = dir_name, size = dir_size_out))
}

dir_size("gbjh", files)

dir_size_tbl <- map_dfr(all_dirs$dir_with_parent, ~dir_size(.x, files))

dirs_below_100k <- dir_size_tbl %>%
  filter(size <= 100000)


dirs_below_100k

sum(dirs_below_100k$size)

free_space <- 70000000 - max(dir_size_tbl$size)

space_reqd <- 30000000 - free_space

dir_size_tbl %>%
  filter(size >= space_reqd) %>%
  arrange(size)


