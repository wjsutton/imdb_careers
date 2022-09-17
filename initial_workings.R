# Find films for the top 100 Actresses

# missing at least 2 actresses
library(dplyr)
library(rvest)
library(stringr)

df <- read.csv("data/actorfilms_data.csv")

# Cate B
# ActorID = nm0000949
filter(df,ActorID == 'nm0000949')

top_100_actors <- read_html("https://www.imdb.com/list/ls063784435/")

actor_id <- top_100_actors %>% 
  html_nodes('.lister-item-image') %>% 
  html_element("a") %>% 
  html_attr("href") 

actor_id <- stringr::str_extract(actor_id,'nm[0-9]+')   
actor_rank <- 1:100

rank_df <- data.frame(id=actor_id, rank=actor_rank,stringsAsFactors = F)

top_100_df <- inner_join(df,rank_df, by = c("ActorID" = "id"))

write.csv(top_100_df,"top_100_actresses.csv",row.names = FALSE)


