### script for setting up analysis

rec = list(
  all_pos = TRUE,
  rec = 0.5, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
  rec_150_yds = 0, rec_200_yds = 0
)

## get data
season <- 2020

ff_projs <- ffanalytics::scrape_data(src = c("CBS","FantasyPros","FantasySharks"),
                            pos = c("QB", "RB", "WR", "TE", "K","DST"),
                            season = 2023,
                            week = 0
                            )



## create projections
#keepers <- c("14104","11675","14797","11244","13132","13404","14802","15254","13164","13589","13130","14832","7836","13319","13131","15256","12626","15262","12801","15281")
keepers <- c("")

agg_ff_projs <- projections_table(ff_projs)

agg_projs_risk<- agg_ff_projs %>% 
  add_ecr() %>% 
  add_uncertainty() %>% 
  add_player_info()

agg_projs_risk %>% 
  select(id, first_name,last_name) %>% 
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



