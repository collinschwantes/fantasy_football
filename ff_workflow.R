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
                            season = 2022,
                            week = 0
                            )
scoring$rec <- rec


## create projections
agg_ff_projs <- projections_table(ff_projs)

agg_projs_risk<- agg_ff_projs %>% 
  add_ecr() %>% 
  add_uncertainty() %>% 
  add_player_info()

agg_projs_risk %>% View()

agg_projs_risk %>% 
  filter(avg_type != "weighted") %>% 
  arrange(tier,sd_pts,ceiling,pos_ecr) %>% 
  select(avg_type,pos,first_name, last_name, team, floor_rank, ceiling_rank,tier, overall_ecr,sd_pts)


### RB ----

agg_projs_risk %>% 
  filter(pos == "RB") %>% 
  filter(avg_type != "weighted") %>% 
  arrange(desc(points),sd_pts,ceiling,pos_ecr) %>% 
  select(avg_type,first_name, last_name, team, floor_rank, ceiling_rank,tier, overall_ecr,sd_pts)


### QB ----

### WR ----

### TE ----

### DST ----

### Draft order --  

