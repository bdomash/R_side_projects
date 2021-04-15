library(tidyverse)
library(lubridate)
library(textutils)
library(furrr)


seasons <- c(1:37)
season_url <- "https://www.j-archive.com/showseason.php?season="

get_games <- function(season_no){
  print(paste("Season: ",season_no,sep=""))
  
  this.url <- paste0(season_url,season_no)
  
  season.df <- read_lines(this.url) %>%
    as.data.frame() %>%
    rename(col = 1) %>%
    filter(col %>% str_detect("game_id=")) %>%
    mutate(col = str_replace_all(col," ","") %>%
             str_replace(.,'<ahref="https://www.j-archive.com/showgame.php\\?game_id=',"")) %>%
    separate(col,c("game_id","rest"),sep = '"') %>%
    separate(rest,c("toss","game_no","toss2","date","toss3"),"(;)|(,)|(<)") %>%
    select(game_id,game_no,date) %>%
    mutate(season = season_no)
  

  return(season.df)
}

#Games with irregular data - a bad way to deal with this but will work until implementing a more robust solution
bad_games <- c("4284", #no data
               "5773", #
               "6232", "6231", "6230", "6229", "6228", "6225", "6224", "6223","6226","6227")

all_games <- map_dfr(seasons,get_games) %>%
  mutate(date = ymd(date)) %>%
  filter(!game_id %>% is.na(),
         !game_no %>% is.na(),
         !game_id %in% bad_games)

game_url = "https://www.j-archive.com/showgame.php?game_id="


get_categories <- function(game_id){
  this.url <- paste0(game_url,game_id)
  
  cats <- read_lines(this.url) %>% 
    as.data.frame() %>%
    rename(col = 1) %>%
    filter(col %>% str_detect("category_name")) %>%
    separate(col,c("t1","category"),'category_name">') %>%
    suppressWarnings(separate(category,"category","<")) %>%
    mutate(category = str_replace(category,"</td></tr>","") %>% HTMLdecode() %>%
             str_replace_all(.,'"',"") %>%
             str_replace_all(.,"</em>","") %>%
             str_replace_all(., "\\s*\\<[^\\)]+\\>", "") %>%
             str_replace_all(.,"\\\\","") %>%
             str_trim()) %>%
    select(category) 
  
  #To deal with tiebreakers
  if(cats %>% nrow() > 13){
    cats <- cats %>% slice(1:13)
  }
  
  cats %>%
    mutate(round = c(rep("J",6),rep("DJ",6),"FJ"),
           col_no = c(rep(1:6,2),""),
           cat_code = if_else(round == "FJ","clue_FJ",paste("clue",round,col_no,sep="_"))) %>%
    select(-col_no)
}

read_game <- function(game_id){
  print(game_id)
  
  this.url <- paste0(game_url,game_id)
  raw_text <-  read_lines(this.url)
  

  category_board <- get_categories(game_id)
  
  
  game_board <- raw_text %>%
    as.data.frame() %>%
    rename(col = 1) %>%
    filter(col %>% str_detect("onmouseover")) %>% 
    mutate(col = str_trim(col) %>%
             str_replace(.,'<div onmouseover="',"")) %>%
    separate(col,c("question","answer"),"onmouseout") %>% 
    separate(question,c("t1","cat_code"),"'",remove = F) %>%
    separate(question,c("t2","question"),'correct_response') %>%
    mutate(question = str_replace(question,"&quot;&gt;","") %>%
             str_replace(.,"&lt;i&gt;","") %>%
             str_replace(.,"&.*","") %>%
             str_trim() %>%
             str_to_title()) %>%
    separate(answer,c("r1","r2","answer"),", '") %>%
    mutate(answer = str_replace(answer,"'\\).*","")) %>%
    mutate(answer = answer %>% HTMLdecode() %>% 
             str_replace_all(., "\\s*\\<[^\\)]+\\>", "") %>%
             str_replace_all(.,"\\\\","") %>%
             str_trim()) %>%
    separate(cat_code,c("clue","round","col","row"),"_") %>%
    mutate(cat_code = if_else(round == "FJ","clue_FJ",paste(clue,round,col,sep="_"))) %>%
    mutate(dollar_amt = case_when(round == "J" & row == 1 ~ 200,
                                  round == "J" & row == 2 ~ 400,
                                  round == "J" & row == 3 ~ 600,
                                  round == "J" & row == 4 ~ 800,
                                  round == "J" & row == 5 ~ 1000,
                                  
                                  round == "DJ" & row == 1 ~ 400,
                                  round == "DJ" & row == 2 ~ 800,
                                  round == "DJ" & row == 3 ~ 1200,
                                  round == "DJ" & row == 4 ~ 1600,
                                  round == "DJ" & row == 5 ~ 2000,
                                  
                                  round == "FJ" ~ NaN)) %>%
    select(question,answer,cat_code,dollar_amt) %>%
    mutate(game_id = game_id)
    
    game_board %>% left_join(category_board) %>% left_join(all_games)
}


game_ids <- all_games %>% filter(season %in% 37) %>% pull(game_id)

plan(multiprocess, workers = 20)

jeopardy_scraped <- future_map_dfr(game_ids,read_game)
