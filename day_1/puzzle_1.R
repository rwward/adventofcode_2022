library(tidyverse)
library(httr)

download.file(url = "https://adventofcode.com/2022/day/1/input", 
              destfile = "aoc_input_1.txt")


httr::GET("https://adventofcode.com/2022/day/1/input")

raw <- readLines("puzzle_1/input_1.txt")

processed <- tibble(calories = raw,
                    elves = character(length(raw)))

elf_number <- 1

for(i in 1:(nrow(processed))){
  if(processed$calories[i] == ""){
    elf_number <- elf_number + 1
  }else{
    processed$elves[i] <- elf_number
  }
}

processed <- processed %>% mutate(elves = as.numeric(elves),
                                  calories = as.numeric(calories)) %>%
  filter(!is.na(calories))

# processed %>% count(is.na(calories), is.na(elves))

processed %>%
  group_by(elves) %>%
  summarize(sum_cals = sum(calories)) %>%
  arrange(desc(sum_cals)) %>%
  head(3) %>%
  summarize(sum(sum_cals))


