library(dplyr)
library(tidytext)
library(janeaustenr)
help(austen_books)
library(widyr)


legislation_parsed <- legislation_df %>%
  mutate(
    text = str_to_lower(text),
    text = str_replace_all(text, "-", " "))



legislation_parsed_sample = legislation_parsed %>%  
  sample_n(100)

legislation_parsed_sample1 =legislation_parsed %>%  
  sample_n(100)%>%
  unnest_tokens(word, text)
  
  legislation_parsed_sample2<- legislation_parsed_sample1%>%
    sample_n(100)
    
    legislation_parsed_sample2<- legislation_parsed_sample2%>%anti_join(stop_words, by = legislation_parsed_sample2$word)
  



Bigram_count = function(data){
austen_bigrams <- data[c("state", "text")] %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)
Bigram_count<- austen_bigrams %>%
  dplyr::count(bigram, sort = TRUE)}


x<- Bigram_count(data_token_list_exp)

austen_section_words2 <- data_token_list_exp %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
  
  austen_section_words2<-austen_section_words2%>%  mutate(section = row_number() %/% 10) 


Count<- austen_section_words2 %>% 
     pairwise_count(word, state, sort = TRUE)            

Count_Corr <- austen_section_words2 %>% 
  add_count(word) %>% 
  filter(n >= 2) %>% 
  select(-n) %>%
  pairwise_cor(word, state, sort = TRUE)
  
