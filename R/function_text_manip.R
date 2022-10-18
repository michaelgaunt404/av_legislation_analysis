#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script holds functions for text cleaning and manipulation.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clean_stopwords = function(data){
  #function unnests tokens, cleans stop words, adn renests data to og form
  # currently requires sting column to be called "text"
  data %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    nest(data = word) %>%
    mutate(text = map(data, unlist),
           text = map_chr(text, paste, collapse = " "))
}

ngram_rec <- function(data, ngram_options) {
  recipe(~ state + summary, data = data) %>%
    step_mutate(summary = str_remove_all(summary, "[:punct:]")) %>%
    step_mutate(summary = str_remove_all(summary, "[:digit:]")) %>%
    step_mutate(summary = replace_contraction(summary)) %>%
    step_mutate(summary = lemmatize_strings(summary)) %>%
    # step_stem(text) %>% #i dont like the results of this, may change
    step_tokenize(summary,
                  token = "ngrams",
                  options = ngram_options
    ) %>%
    step_tf(summary)
}

ngram_options = list(n = 2, n_min = 2, lowercase = T
                     ,stopwords = stop_words$word)

recipe = ngram_rec(data = data_av_leg_sum, ngram_options = ngram_options)

data_test = recipe %>%  prep() %>%  juice() %>%
  pivot_longer(cols = starts_with("tf")) %>%
  mutate(count = 1) %>%
  count_percent_zscore(grp_c = c(name)
                       ,grp_p = c()
                       ,col = value) %>%
  arrange(desc(count)) %>%
  view()


data_test %>%
  filter(value != 0) %>%
  bind_log_odds(state, name, value) %>%
  arrange(desc(log_odds_weighted))


##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































