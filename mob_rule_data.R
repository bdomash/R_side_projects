library(rvest)
library(tigris)
library(ggnewscale)
#library(ggpattern)


user_page_raw <- readLines("http://www.mob-rule.com/scoreboard.html")

temp <- user_page_raw %>%
  as.data.frame() %>%
  rename(temp = 1) %>%
  mutate(temp = str_trim(temp)) %>%
  filter(temp %>% str_detect('<td align=\\\"center\\\">')) %>%
  mutate(temp = str_replace(temp,'<td align=\\\"center\\\">',"") %>%
           str_replace_all('</a></td>',"")) %>%
  separate(temp,c("temp2","value"),sep=">",remove = F) %>%
  mutate(value = str_replace(value,'<td',"")) %>%
  mutate(col = rep(c("username","full_name","home_town","n_counties"),nrow(.)/4)) %>%
  select(raw_line = temp,col,value) #%>%
  #mutate(value = if_else(col == "username" & value == "",raw_line %>% str_replace("</td>",""),value))
  

user_page_clean <- data.frame(username = temp %>% filter(col == "username") %>% pull(value),
           full_name =  temp %>% filter(col == "full_name") %>% pull(value),
           town_state = temp %>% filter(col == "home_town") %>% pull(value),
           n_counties =  temp %>% filter(col == "n_counties") %>% pull(value)) %>%
  separate(town_state,c("home_town","home_state"),", ",remove = F) %>%
  mutate(counties_denom = 3143,
         n_counties = as.numeric(n_counties),
         pct_counties = n_counties/counties_denom * 100,
         n_users = nrow(.)) 

state_counts <- user_page_clean %>% mutate(n_counties = as.numeric(n_counties)) %>% group_by(home_state) %>% 
  summarise(avg_counties = mean(n_counties,na.rm = T),
            n_users = n()) %>% 
  filter(home_state %in% state.abb) 

read.csv("http://www.mob-rule.com/show/bdomash/USA.html",header = F) %>%
  rename(county = V1, state = V2,country = V3) %>%
  mutate(county = str_replace(county,'\t',''))


get_users_counties = function(username){
  read.csv(glue("http://www.mob-rule.com/show/{username}/USA.html"),header = F) %>%
    mutate(user = username)
    #rename(county = V1, state = V2,country = V3) %>%
    #mutate(county = str_replace(county,'\t',''),
    #       user = username) 
}

plan(multiprocess, workers= 20)
all_counties_raw <- future_map_dfr(user_page_clean %>% filter(username != "") %>% pull(username),get_users_counties)

all_counties <- all_counties_raw %>%
  rename(county = V1, state = V2, country = V3) %>%
  mutate(county = str_replace(county,'\t','')) %>%
  mutate_at(c("state","country"),~str_trim(.x,"both")) %>%
  as_tibble() %>%
  select(-V0) %>%
  left_join(user_page_clean %>% 
              select(user = username,user_home_town = home_town,user_home_state = home_state,user_counties_visited = n_counties,
                     user_pct_counties = pct_counties,n_users))


#### PART 1: How many users have been to each state? ####

#collapse on state
states_collapsed <- all_counties %>% 
  distinct(user,user_home_state,n_users,state) %>%
  group_by(n_users,state) %>%
  summarise(n_state = n()) %>%
  mutate(pct_state = n_state/n_users) %>%
  arrange(desc(pct_state)) %>%
  mutate(rank = row_number()) %>%
  left_join(states_borders,.) 

#subsets what % of users have been to each state on the state they are from
rank_state <- function(state){
  df <- all_counties %>%
    filter(!state %>% is.na()) %>%
    filter(user_home_state == !!state) %>%
    mutate("users_{state}" := user_page_clean %>% filter(home_state == !!state) %>% nrow()) %>%
    distinct(user,!!sym(glue("users_{state}")),state) %>%
    group_by(!!sym(glue("users_{state}")),state) %>%
    summarise("n_state_{state}" := n()) %>%
    mutate("pct_state_{state}" := !!sym(glue("n_state_{state}"))/!!sym(glue("users_{state}"))) %>%
    arrange(desc(!!sym(glue("pct_state_{state}")))) %>%
    mutate("rank_{state}" := row_number(),
           "pct_state_{state}_binned" := cut(!!sym(glue("pct_state_{state}")),seq(0,1,.1),include.lowest = T)) %>%
    arrange(state) %>%
    ungroup()
  
  return(df)
}


states_ranked_all <- map(c(state.abb,"DC"),rank_state) %>%
  reduce(left_join,by="state")

#Master states DF that has where everyone from each state ranks in each in terms of visited
states_collapsed <- states_collapsed %>% left_join(states_ranked_all)
states_collapsed <- states_collapsed %>% 
  mutate(pct_state_binned = cut(pct_state,seq(0,1,.1),include.lowest = T))

#Other way around - where do each states ranked in terms of people from other states
states_collapsed %>% as_tibble() %>% select(state_name,starts_with("rank")) %>%
  arrange(state_name) %>%
  column_to_rownames("state_name") %>% t() %>% as.data.frame() %>%
  bind_rows(states_collapsed %>% as_tibble() %>% select(state_name,starts_with("rank")) %>%
              arrange(state_name) %>%
              column_to_rownames("state_name") %>% t() %>% as.data.frame() %>% slice(-1) %>% summarise_all(mean)) %>% 
  rownames_to_column() %>%
  rename(state = rowname) %>%
  mutate(state = if_else(state == "...52","State Average",state)) %>%
  view()

ggplot(states_collapsed %>% filter(lower48)) +
  geom_sf(aes(fill = pct_state_CA_binned),color = "black") +
  scale_fill_brewer(palette = "RdYlBu",direction = -1, name = "Proportion of Mob-Rule\nUserbase Visited") +
  theme_void()
  


states_collapsed %>% as_tibble() %>% select(state_name,starts_with("rank")) %>%
  arrange(state_name) %>%
  column_to_rownames("state_name") %>% t() %>% as.data.frame() %>% slice(-1) %>% summarise_all(mean) %>%
  as_tibble() %>% t() %>% View()

counties_collapsed <- all_counties %>% 
  group_by(county,state,n_users) %>% 
  summarise(n_visited = n(),brandon = sum(if_else(user == "bdomash",1,0))) %>%
  mutate(pct_visited = n_visited/n_users) %>%
  ungroup() %>%
  arrange(desc(pct_visited)) %>%
  mutate(rank = row_number(),
         county_state = paste0(county,"_",state) %>% tolower()) %>%
  filter(!county %>% is.na())

il <- all_counties %>% 
  filter(user_home_state == "IL") %>%
  mutate(n_users = user_page_clean %>% filter(home_state == "IL") %>% nrow()) %>% 
  group_by(county,state,n_users) %>% 
  summarise(n_visited = n(),brandon = sum(if_else(user == "bdomash",1,0))) %>%
  mutate(pct_visited = n_visited/n_users) %>%
  ungroup() %>%
  arrange(desc(pct_visited)) %>%
  mutate(rank = row_number(),
         county_state = paste0(county,"_",state) %>% tolower()) %>%
  filter(!county %>% is.na()) %>%
  select(county_state,il_visited = n_visited,il_pct_visited = pct_visited,il_rank = rank)


md <- all_counties %>% 
  filter(user_home_state == "MD") %>%
  mutate(n_users = user_page_clean %>% filter(home_state == "MD") %>% nrow()) %>% 
  group_by(county,state,n_users) %>% 
  summarise(n_visited = n(),brandon = sum(if_else(user == "bdomash",1,0))) %>%
  mutate(pct_visited = n_visited/n_users) %>%
  ungroup() %>%
  arrange(desc(pct_visited)) %>%
  mutate(rank = row_number(),
         county_state = paste0(county,"_",state) %>% tolower()) %>%
  filter(!county %>% is.na()) %>%
  select(county_state,md_visited = n_visited,md_pct_visited = pct_visited,md_rank = rank)

wa <- all_counties %>% 
  filter(user_home_state == "WA") %>%
  mutate(n_users = user_page_clean %>% filter(home_state == "WA") %>% nrow()) %>% 
  group_by(county,state,n_users) %>% 
  summarise(n_visited = n(),brandon = sum(if_else(user == "bdomash",1,0))) %>%
  mutate(pct_visited = n_visited/n_users) %>%
  ungroup() %>%
  arrange(desc(pct_visited)) %>%
  mutate(rank = row_number(),
         county_state = paste0(county,"_",state) %>% tolower()) %>%
  filter(!county %>% is.na()) %>%
  select(county_state,wa_visited = n_visited,wa_pct_visited = pct_visited,wa_rank = rank)
  
ma <- all_counties %>% 
  filter(user_home_state == "MA") %>%
  mutate(n_users = user_page_clean %>% filter(home_state == "MA") %>% nrow()) %>% 
  group_by(county,state,n_users) %>% 
  summarise(n_visited = n(),brandon = sum(if_else(user == "bdomash",1,0))) %>%
  mutate(pct_visited = n_visited/n_users) %>%
  ungroup() %>%
  arrange(desc(pct_visited)) %>%
  mutate(rank = row_number(),
         county_state = paste0(county,"_",state) %>% tolower()) %>%
  filter(!county %>% is.na()) %>%
  select(county_state,ma_visited = n_visited,ma_pct_visited = pct_visited,ma_rank = rank)

counties_collapsed %>% filter(brandon == 1) %>% View()

#Load in county shapefile
states_all <- tigris::states() %>%
  as_tibble() %>%
  select(STATEFP,state_name = NAME) %>%
  mutate(state_name = if_else(STATEFP == "11","DC",state_name))

state_map <- data.frame(state = state.abb,state_name = state.name) %>%
  add_row(state = "DC", state_name = "DC")

states_borders <- tigris::states() %>%
  select(state = STUSPS) %>%
  inner_join(state_map) %>%
  mutate(lower48 = if_else(state %in% c("AK","HI"),F,T))

counties_all <- tigris::counties() %>%
  bind_rows(tigris::county_subdivisions(state = "AK") %>%
              filter(NAME %in% c("Copper River","Chugach"))) %>%
  select(STATEFP,county_name = NAME,county_full_name = NAMELSAD) %>%
  left_join(states_all) %>%
  left_join(state_map) %>%
  filter(!state %>% is.na()) %>%
  mutate(county_name = if_else(state == "DC","Washington",county_name %>% stringi::stri_trans_general("Latin-ASCII")),
         county_name = if_else(county_full_name %>% str_detect("city"),paste0("City of ",county_name),county_name),
         county_state = paste0(county_name,"_",state) %>% tolower()) %>%
  filter(county_name != "Valdez-Cordova") %>%
  left_join(counties_collapsed %>% select(n_visited,brandon,pct_visited,rank,county_state)) %>%
  left_join(il) %>%
  left_join(wa) %>%
  left_join(ma) %>%
  left_join(md) %>%
  mutate(wa_pct_visited = replace_na(wa_pct_visited,0),
         ma_pct_visited = replace_na(ma_pct_visited,0),
         md_pct_visited = replace_na(md_pct_visited,0)) %>%
  mutate(pct_visited_cat = cut(.$pct_visited,seq(0,1,.1),dig.lab =2),
         brandon = as.factor(brandon),
         lower48 = if_else(state %in% c("AK","HI"),F,T),
         pct_visited_cat_il = cut(.$il_pct_visited,seq(0,1,.1),dig.lab =2),
         pct_visited_cat_wa = cut(.$wa_pct_visited,seq(0,1,.1),dig.lab =2,include.lowest = T),
         pct_visited_cat_ma = cut(.$ma_pct_visited,seq(0,1,.1),dig.lab =2,include.lowest = T),
         pct_visited_cat_md = cut(.$md_pct_visited,seq(0,1,.1),dig.lab =2,include.lowest = T)) %>%
  arrange(rank)
  


counties_all %>% as_tibble() %>% select(-geometry) %>% view() #filter(pct_visited_cat_wa %>% is.na()) %>% View()
counties_all %>% left_join(counties_collapsed %>% select(n_visited,brandon,pct_visited,rank,county_state),"county_state") %>%
  as_tibble() %>% select(-geometry) %>%
#Lets load in highways
load_highways <- function(state){
  tigris::primary_secondary_roads(state) %>% filter(FULLNAME %>% str_detect("I-") | FULLNAME %>% str_detect("US ")) %>%
    mutate(st = state)
}

us_highways <- future_map_dfr(state.abb,load_highways)
highways_clean <- us_highways %>%
  mutate(type = if_else(FULLNAME %>% str_detect("I-"),"Interstate","Highway"),
         lower48 = if_else(st %in% c("AK","HI"),F,T))

interstates <- highways_clean %>% filter(type == "Interstate")
highways <- highways_clean %>% filter(type == "Highway")
interstates %>%  filter(lower48) %>% mapview()

#Plot in ggplot
ggplot(counties_all %>% filter(lower48)) +
  #geom_sf(aes(fill = pct_visited_cat_wi),color = "gray30") +
  scale_fill_brewer(palette = "RdYlBu",direction = -1, name = "Proportion of Mob-Rule\nUserbase Visited") +
  #geom_sf(data = highways %>% filter(lower48),color = "chocolate",size = .2) +
  #geom_sf(data = interstates %>% filter(lower48),color = "brown",size = .4) +
  geom_sf(data = states_borders %>% filter(lower48),fill = NA,color = "grey20",size = 1.5) +
  geom_sf(data = counties_all %>% filter(brandon == 1), aes(color = brandon),fill = NA, size = 1) +
  #scale_color_manual(values = c("black"),
   #                  labels = c("Yes"),
    #                 name = "Has Brandon Visited?") +
  theme_void()

temp_palette <-  colorRampPalette(c("white","blue","orange","red"),bias = 1.5); temp_palette(10)

#Just folks from Illinois
ggplot(counties_all %>% filter(lower48)) +
  geom_sf(aes(fill = pct_visited_cat_md),color = "gray30") +
  #scale_fill_manual(values = temp_palette(10),name = "Proportion of Mob-Rule\nUserbase from\nIL Visited") +
  scale_fill_brewer(palette = "RdYlBu",direction = -1, name = "Proportion of Mob-Rule\nUserbase from\nMD Visited") +
  #geom_sf(data = highways %>% filter(lower48),color = "chocolate",size = .4) +
  #geom_sf(data = interstates %>% filter(lower48),color = "brown",size = .4) +
  geom_sf(data = states_borders %>% filter(lower48),fill = NA,color = "gray20",size = 1.5) +
  #geom_sf(data = counties_all %>% filter(brandon == 1), aes(color = brandon),fill = NA, size = 1) +
  #scale_color_manual(values = c("black"),
  #                   labels = c("Yes"),
  #                   name = "Has Brandon Visited?") +
  theme_void()

counties_all %>% as_tibble() %>% 
  select(county_name,state,pct_visited,rank,brandon,il_pct_visited,il_rank) %>%
  filter(brandon == 1) %>%
  arrange(il_rank) %>% View()


