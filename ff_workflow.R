source("packages.R")

### script for setting up analysis

rec = list(
  all_pos = TRUE,
  rec = 0.5, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
  rec_150_yds = 0, rec_200_yds = 0
)

## get data
season <- 2021

ff_projs <- ffanalytics::scrape_data(src = c("CBS","FantasyPros","FantasySharks"),
                            pos = c("QB", "RB", "WR", "TE", "K","DST"),
                            season = 2024,
                            week = 0
                            )


#   
# keepers <- c(
# 3  "Bijan Robinson",
# 4 "Jahmyr Gibbs",
# 5 "Saquon Barkely",
# 6 "Christian McCarphy",
# 7 "Achane",
# 8 "Derrick Henry",
# 9 "Josh Jacobs",
# 10 "Jonathan Taylor",
# 11 "Kyren Williams",
# 12 "Ja'Marr Chase",
# 13 "Justin Jefferson",
# 14 "Ceedee Lamb",
# 15 "Puka Macua",
# 16 "Amon-Ra St. Brown",
# 17 "Nico Collins",
# 18 "Brian Thomas Jr.",
# 19 "Tyreek Hill",
# 2 "Josh Allen",
# 1 "Jalen Hurts",
# 20 "Drake London")

length(keepers)

## create projections
keepers <- c("13589","14783","16161","16162","13604",
             "13130","16167","12626","14073","15710",
             "14802","15281","14836","14832", "16211",
             "15287","15290","16618","12801","15751" )


agg_ff_projs <- projections_table(ff_projs)

agg_projs_risk<- agg_ff_projs %>% 
  add_ecr() %>% 
  add_uncertainty() %>% 
  add_player_info()

agg_projs_risk %>% 
  select(id, first_name,last_name) %>% 
  View()


agg_projs_risk %>% 
  arrange(tier,desc(points),sd_pts,ceiling,pos_ecr) %>% 
  select(first_name,last_name,id)%>% 
  distinct(id,.keep_all = TRUE) %>% 
  View()



agg_projs_risk %>% 
  filter(avg_type == "robust") %>% 
  filter(!id %in% {{ keepers }} ) %>% 
  arrange(tier,desc(points),sd_pts,ceiling,pos_ecr) %>% 
  mutate(name = paste(first_name, last_name)) %>% 
  select( pos, name, team, floor_rank, ceiling_rank,tier, pos_ecr,overall_ecr,sd_pts) %>% 
  write.csv("data/overall.csv")


### write csvs ----

dir.create("data",recursive = T)

positions<- c("RB","WR","QB","TE","K","DST")

for(i in positions){
  
  agg_projs_risk %>% 
    filter(pos == i) %>% 
    filter(!id %in% {{ keepers }} ) %>% 
    filter(avg_type == "robust") %>% 
    arrange(tier,desc(points),sd_pts,ceiling,pos_ecr) %>% 
    select(first_name, last_name, team, floor_rank, ceiling_rank,tier, pos_ecr,overall_ecr,sd_pts) %>%
    write.csv(file = sprintf("data/%s.csv",i))
}



