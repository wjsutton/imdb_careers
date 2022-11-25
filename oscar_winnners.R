# Find oscar winners

# Missing 11 actresses
library(dplyr)
library(rvest)
library(stringr)

df <- read.csv("the_oscar_award.csv")

actresses <- filter(df,category == 'ACTRESS')
actresses <- filter(actresses,winner == 'True')

oscar_winners <- unique(actresses$name)


actor_df <- read.csv("data/actorfilms_data.csv")

oscar_winning_actresses <- filter(actor_df,Actor %in% oscar_winners)

write.csv(oscar_winning_actresses,"top_100_actresses.csv",row.names = FALSE)