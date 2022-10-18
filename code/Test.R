legislation_df = 
  here("data/legsilation_df_2021-06-24.rds") %>% 
  readRDS()

legislation_df_sample = legislation_df %>%  
  sample_n(100) %>%  
  filter(str_count(text)<10000)

## Intrduction

#<!--#SECTION NAME===============================================================
#use this header to make demarcations/section in code [delete this line]
#short description -->

## Basic Data Exploration 

#Preping data some more... good place to copy and build analyses off of. 

token_df = legislation_df_sample %>% 
  clean_stopwords() %>%  #removes stop words, custom function


token_df_agg = token_df %>% 
  count(state, bill_name) 