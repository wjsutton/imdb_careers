# Get and download IMDB data
# https://datasets.imdbws.com/

# IMDB Data Dictionary
# https://www.imdb.com/interfaces/

# IMDB Series IDs
#  - Top Gear: tt1628033
#  - Grand Tour: tt5712554

# IMDB Downloads
#  - title.basics.tsv.gz
#  - title.episode.tsv.gz
#  - title.ratings.tsv.gz

library(dplyr)
library(readr)
library(tidyr)
library(R.utils)

gunzip("data/name.basics.tsv.gz", remove=FALSE)
gunzip("data/title.ratings.tsv.gz", remove=FALSE)
gunzip("data/title.principals.tsv.gz", remove=FALSE)
gunzip("data/title.basics.tsv.gz", remove=FALSE)


act_name <- read_delim('data/name.basics.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')
title_princ <- read_delim('data/title.principals.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')
title_rate <- read_delim('data/title.ratings.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')
title_basics <- read_delim('data/title.basics.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')

df <- read.csv("the_oscar_award.csv")

movies <- filter(title_basics,titleType == 'movie')
avg_rating_by_year <- inner_join(title_rate,movies[,c('tconst','startYear')],by = c('tconst' = 'tconst'))
#avg_rating_by_year$rating_and_votes <- avg_rating_by_year$averageRating * avg_rating_by_year$numVotes

#avg_rating_df <- avg_rating_by_year %>% 
#  group_by(startYear) %>%
#  summarise(total_rating_and_votes = sum(rating_and_votes),
#            total_votes = )

avg_rating_by_year <- filter(avg_rating_by_year,numVotes >= 100)

avg_rating_df <- avg_rating_by_year %>% 
    group_by(startYear) %>%
    summarise(averageRating = mean(averageRating))
names(avg_rating_df) <- c('startYear','yearly_avg_rating')

avg_rating_overall <- mean(avg_rating_by_year$averageRating)


actresses <- filter(df,category %in% c('ACTRESS','ACTRESS IN A LEADING ROLE'))
actresses <- filter(actresses,winner == 'True')
actresses$oscar <- 1 



oscar_winners <- unique(actresses$name)


oscar_winning_actresses <- filter(act_name,primaryName %in% oscar_winners)

oscar_winning_actresses <- oscar_winning_actresses[grepl('actress',oscar_winning_actresses$primaryProfession),]
oscar_winning_actresses <- oscar_winning_actresses[grepl('tt',oscar_winning_actresses$knownForTitles),]

oscar_df <- inner_join(oscar_winning_actresses,title_princ, by = c('nconst' = 'nconst'))
oscar_df <- inner_join(oscar_df,title_rate, by = c('tconst' = 'tconst'))
oscar_df <- inner_join(oscar_df,title_basics, by = c('tconst' = 'tconst'))

oscar_df <- filter(oscar_df,category %in% c("actress","self"))
oscar_df <- filter(oscar_df,titleType == 'movie')

actress_dedup <- oscar_df %>% group_by(primaryName,nconst) %>% tally() %>% arrange(primaryName)
actress_dedup$rn <- ave(actress_dedup$nconst,actress_dedup$primaryName, FUN = seq_along)
actress_dedup <- filter(actress_dedup,rn == 1)

oscar_df <- filter(oscar_df,nconst %in% actress_dedup$nconst)
oscar_df$age_at_filming <- oscar_df$startYear - oscar_df$birthYear
oscar_df <- left_join(oscar_df,actresses, c('primaryTitle' = 'film','startYear' = 'year_film','primaryName' = 'name'))
oscar_df$oscar <- ifelse(is.na(oscar_df$oscar),0,1)

oscar_df <- inner_join(oscar_df,avg_rating_df, by = c('startYear' = 'startYear'))
oscar_df$overall_avg_rating <- avg_rating_overall

oscar_df$rating_score <- ifelse(oscar_df$averageRating>=oscar_df$yearly_avg_rating,1,-1)


#age_at_filming <- 0:100
#nconst <- unique(oscar_df$nconst)
#age_at_filming <- rep(age_at_filming,length(nconst))
#scaffold <- as.data.frame(cbind(age_at_filming,nconst))
#scaffold$age_at_filming <- as.integer(scaffold$age_at_filming)
#oscar_df <- left_join(scaffold,oscar_df, by = c('nconst' = 'nconst', 'age_at_filming' = 'age_at_filming' ))


typical_df <- oscar_df[,c("age_at_filming","nconst","oscar","startYear","averageRating","yearly_avg_rating")]
typical_df$net_rating <- typical_df$averageRating - typical_df$yearly_avg_rating
typical_df$film <- 1

typical_career <- typical_df %>% 
  group_by(age_at_filming) %>%
  summarise(total_net_rating = sum(net_rating),
            total_actresses = n_distinct(nconst),
            total_oscars = sum(oscar),
            total_films = sum(film))

typical_career$avg_net_rating <- typical_career$total_net_rating / 76
typical_career$avg_films <- typical_career$total_films  / 76
typical_career$oscar <- typical_career$total_oscars / 76

write.csv(typical_career,"actresses_typical_career.csv",row.names = FALSE)


write.csv(oscar_df,"top_100_actresses.csv",row.names = FALSE)
write.csv(oscar_winning_actresses,"actresses_check.csv",row.names = FALSE)
