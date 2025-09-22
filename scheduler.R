library(dplyr)

div_a <- c("binford","polish","loblaws","fire","flavortown")

div_b <- c("salts","moon","eskimo","gov","loco")

# already played 
div_out_already <- c("fire gov", "polish eskimo","loblaws moon",
                     "fire salts","binford loco", "polish gov",
                     "loblaws eskimo", "flavortown moon","fire loco",
                     "loblaws gov","binford moon", "polish salts",
                     "flavortown eskimo")
div_in_already <-c("loco salts","binford flavortown")

### set to character
div_a_in <- expand.grid(div_a,div_a) |>
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2)) |>  filter(Var1 != Var2) |>
  mutate(pair = paste(Var1, Var2,sep = " ")) |>
  dplyr::filter(!pair %in% div_in_already)

div_b_in <- expand.grid(div_b,div_b)|>
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2)) |>  filter(Var1 != Var2) |>
  mutate(pair = paste(Var1, Var2,sep = " ")) |>
  dplyr::filter(!pair %in% div_in_already)

div_out <- expand.grid(div_a,div_b)|>
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2)) |>
  mutate(pair = paste(Var1, Var2,sep = " ")) |>
dplyr::filter(!pair %in% div_out_already)

## draw 2 from div_a_in without replacement
## draw 2 from div_b_in wihtout replacement
## draw 1 from div out without replacement

 
schedule <- data.frame(week = numeric(), pair = character(), in_division= logical() ) 

for(week in 4:14){

  print(week)
  df_week <- data.frame(week = numeric(), pair = character(), in_division= logical() )  
  
  ## div a
  ## draw 2 from div_a_in without replacement
  len_a <- nrow(div_a_in)
  if(len_a <= 1){
    print("only 1")
    pair_index_a <- 1
    
    pair_df_a <- div_a_in[pair_index_a,]
    
    ineligible_out_a <-c(pair_df_a$Var1,pair_df_a$Var2)
    
    df_week_a <- data.frame(week = week, pair = pair_df_a$pair, in_division= TRUE ) 
    
    ### remove from div_a_in
    
    div_a_in <- div_a_in[-pair_index_a,]
    
  } else {
    
    df_week_a <- data.frame(week = rep(week,2), pair = 2, in_division= TRUE ) 
    ineligible_out_a <- c()
    for(i in 1:2){
      
      len_a <- nrow(div_a_in)
      pair_index_a <- sample(1:len_a,size = 1,replace = FALSE)  
      
      pair_df_a <- div_a_in[pair_index_a,]
      
      ineligible_out_a<- c(ineligible_out_a,c(pair_df_a$Var1,pair_df_a$Var2))
      
      df_week_a[i,] <- data.frame(week = week, pair = pair_df_a$pair, in_division= TRUE ) 
      ### remove from div_a_in
      div_a_in <- div_a_in[-pair_index_a,]
    }
  }
  


  
  # div b
  ## draw 2 from div_b_in wihtout replacement
  len_b <- nrow(div_b_in)
  if(len_b == 1){
    pair_index_b <- 1
  } else {
    pair_index_b <- sample(1:len_b,size = 2,replace = FALSE)
  }

  pair_df_b <- div_b_in[pair_index_b,]
  
  ineligible_out_b <-c(pair_df_b$Var1,pair_df_b$Var2)
  
  df_week_b <- data.frame(week = week, pair = pair_df_b$pair, in_division= TRUE ) 
  ### remove from div_a_in
  
  div_b_in <- div_b_in[-pair_index_b,]
  
  # out
  ## draw 1 from div out without replacement
  
  ineligible_out_ab <- c(ineligible_out_a,ineligible_out_b)
  
  week_subset <- div_out |>
    dplyr::filter(!Var1 %in% ineligible_out_ab | !Var2 %in% ineligible_out_ab)
  
  len_out <- nrow(week_subset)
  
  if(len_out == 0){
    print("no more out games")
    df_week_out <- data.frame(week = week, pair = "", in_division= FALSE ) 
    
  } else {
    print("still have teams?")
    print(pair_df_out$pair)
    pair_index_out <- sample(1:len_out,size = 1,replace = FALSE)
    pair_df_out <- week_subset[pair_index_out,]
    
    df_week_out <- data.frame(week = week, pair = pair_df_out$pair, in_division= FALSE ) 
    ### remove from div_a_in
    
    div_out <- div_out[-pair_index_b,]
  }
  
  
  ## rbind a b and out
  week_schedule <- rbind(df_week_out,df_week_b,df_week_a)
  
  if(nrow(week_schedule) < 5){
    diff_week <- 5-nrow(week_schedule)
    df_week_diff<- data.frame(week = rep(week,diff_week), pair = "", in_division= FALSE ) 
    week_schedule<- rbind(week_schedule,df_week_diff)
  }
  
  schedule <- rbind(schedule,week_schedule)
}

schedule
