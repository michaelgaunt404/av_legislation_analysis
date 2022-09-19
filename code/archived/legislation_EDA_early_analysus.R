#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is [[insert desciption here - what it does/solve]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert breif readme here]]
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(rvest)
# library(httr)
# library(XML)
# library(RSelenium)
library(tidyverse)
library(data.table)
library(lubridate)
library(textrecipes)
library(textfeatures)
library(textclean)
library(tidytext)
# library(future)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project file performs this task - section is not required

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#zz_localG performs this task - section is not required

#CODE BODY START================================================================

#data set up====================================================================
bill_data = readRDS(here::here(
  "data/scraped_legislation_2021-06-18.rds")
)
#don't have to run this until the data is scraped again
# big_list_node_text = readRDS(here::here("data/big_list_node_text.rds"))
#
# big_list_node_td = readRDS(here::here("data/big_list_node_td.rds"))
#

legsilation_df = tibble(state = bill_data$state[-c(497:498)],
       bill_name = bill_data$bill_name[-c(497:498)],
       text = unlist(big_list_node_text))
#
# legsilation_df %>%
#   saveRDS(str_glue("./data/legsilation_df_{Sys.Date()}.rds"))

legsilation_df = readRDS("./data/legsilation_df_2021-06-24.rds")

#EDA============================================================================

tmp = big_list_node_td %>%
  unlist() %>%
  matrix(ncol = 2, byrow = T) %>%
  data.table() %>%
  set_names(c("bill_name", "info")) %>%
  .[,`:=`(author = gsub("Version.*", "\\1", info) %>%
            gsub(".*: ", "\\1", .),
          status = gsub("Version Date.*", "\\1", info) %>%
            gsub(".*Version: ", "\\1", .),
          date = gsub(".*Date: ", "\\1", info) %>%
            mdy())] %>%
  print()

tmp %>%  filter(str_detect(author, "pp"))

legsilation_df = legsilation_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  nest(word) %>%  mutate(text = map(data, unlist),
                         text = map_chr(text, paste, collapse = " "))

FL S 2500
legsilation_df %>%
  filter(state == "FL"
         , bill_name == "FL S 660"
         )

legsilation_df %>%
  filter(state == "FL"
         , bill_name == "FL S 2500"
  )

bill_data %>%  filter(state == "FL"
                  , bill_name == "FL S 2500"
)


legsilation_df %>%
unnest_tokens(word, text) %>%

  count(state, bill_name, word, sort = TRUE) %>%
  filter(state == "FL"
         , bill_name == "FL S 2500"
  ) %>%
  bind_tf_idf(word, bill_name, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(state) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = state)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~state, ncol = 4, scales = "free") +
  labs(x = "tf-idf", y = NULL)

