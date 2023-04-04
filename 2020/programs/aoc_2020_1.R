# --- Day 1: Report Repair ---
#   After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a 
# tropical island. Surely, Christmas will go on without you.
# 
# The tropical island has its own currency and is entirely cash-only. The gold coins used there have a 
# little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to 
# have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you 
# can pay the deposit on your room.
# 
# To save your vacation, you need to get all fifty stars by December 25th.
# 
# Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; 
# the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
#   
#   Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); 
# apparently, something isn't quite adding up.
# 
# Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers 
# together.
# 
# For example, suppose your expense report contained the following:
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, 
# so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply 
# them together?

# load packages
library("tidyr")
library("dplyr")
# require(plyr)
# require(dplyr)


# # read in data
my_data <- read.table(file.path("raw", "aoc_2020_1_input.txt"), blank.lines.skip = F) %>%
  rename(expense=V1)

# # sample data frame
# my_data <- structure(list(expense = c(
#   20L, 24L, 32L, 22L, 19L, 25L, 28L,
#   22L, 18L, 21L
# )),
# class = "data.frame",
# row.names = c(NA, -10L))

# count of expense
n <- nrow(my_data)
n

# create all possible combinations of 2 to get sum
comb <- combn(my_data$expense, 2)
comb

# transpose all possible combinations into 2 columns
tcomb <- as_tibble(t(comb))
tcomb

# sum the columns
tcombsum <- tcomb %>% mutate(sum = V1+V2) 
tcombsum

# find sum that equals the number you want 
want <- tcombsum %>% filter(sum==2020)
want

# multiply those 2 numbers
fix <- want %>% mutate(product = V1*V2)
fix$product

# --- Part Two ---
# The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over 
# from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the 
# same criteria.
# 
# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces 
# the answer, 241861950.
# 
# In your expense report, what is the product of the three entries that sum to 2020?

# create all possible combinations of 3 to get sum
comb3 <- combn(my_data$expense, 3)
comb3

# transpose all possible combinations into 2 columns
tcomb3 <- as_tibble(t(comb3))
tcomb3

# sum the columns
tcombsum3 <- tcomb3 %>% mutate(sum = V1+V2+V3) 
tcombsum3

# find sum that equals the number you want 
want3 <- tcombsum3 %>% filter(sum==2020)
want3

# multiply those 2 numbers
fix3 <- want3 %>% mutate(product = V1*V2*V3)
fix3$product
