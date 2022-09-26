library(rvest)
library(httr)
library(XML)
library(RSelenium)
library(tidyverse)
library(data.table)
library(lubridate)
library(future)

#start driver
rD <- rsDriver(browser = 'firefox', port = 4444L)

# Assign the client
remDr <- rD$client

#open client
remDr$open()

# Navigate to website
appurl <- "https://www.ncsl.org/research/transportation/autonomous-vehicles-legislative-database.aspx"
remDr$navigate(appurl)

browser$findElement(using = "xpath", str_glue("//select[@id = 'years']/option[{.x}]"))$clickElement()

scraped_raw_html =
  remDr$getPageSource()[[1]] %>%
  read_html() %>%
  rvest::html_nodes(".ModNCSLStateNetC div div+ div a")

bill_data = data.table(bill_name = scraped_raw_html %>%
                html_text(),
              bill_html = scraped_raw_html %>%
                html_attr('href')) %>%
  .[,`:=`(state = gsub(" .*", "\\1", bill_name),
    bill_name = str_squish(bill_name))] %>%
  .[-1,]

saveRDS(bill_data, here::here(
  str_glue("data/scraped_legislation_{Sys.Date()}.rds")
  ))

bill_data = read_rds(here::here(
  "data/scraped_legislation_2021-06-18.rds")
)

scrape_av = function(url){
  xml2::read_html(url)
  # %>%
  #   rvest::html_nodes(".text")
}

safe_scrape_av = safely(scrape_av)

good_results_only = function(data){
  data %>%
    .["result"] %>%
    .[[1]]
}

time = .01
tictoc::tic()
# large_html_scrape = bill_data %>%
#   .[1:10,] %>%
#   .$bill_html %>%
#   map(~{
#     Sys.sleep(5)
#     safe_scrape_av(.x) %>%
#       good_results_only()
#   })
large_html_scrape_text = bill_data %>%
  .[21:26,] %>%
  .$bill_html %>%
  map(~{
    Sys.sleep(5)
    safe_scrape_av(.x) %>%
      good_results_only()
  })
tictoc::toc()

install.packages("urltools")
library(robotstxt)
robotstxt::get_robotstxt(urltools::domain("http://custom.statenet.com/")) %>%  cat()

library(furrr)

tictoc::tic()
big_list_scraped_html = bill_data$bill_html %>%
  future_map(~safe_scrape_av(.x) %>%
        good_results_only())
tictoc::toc()

big_list_scraped_html %>%
  length()

big_list_node_text = big_list_scraped_html %>%
  map(~html_nodes(.x, ".text") %>%
        html_text())
saveRDS(big_list_node_text, here::here("data/big_list_node_text.rds"))
big_list_node_text[2]

big_list_node_td = big_list_scraped_html %>%
  map(~html_nodes(.x, "#text-identifier td") %>%
        .[1:2] %>%
        html_text())

saveRDS(big_list_node_td, here::here("data/big_list_node_td.rds"))


big_list_node_td[2]

big_list_node_text = readRDS(here::here("data/big_list_node_text.rds"))

big_list_node_td = readRDS(here::here("data/big_list_node_td.rds"))

combined_scrape = Map(c,
    big_list_node_td ,
    big_list_node_text) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  data.table() %>%
  set_names("bill_name", "info", "length") %>%
  .[,`:=`(author = gsub("Version.*", "\\1", info) %>%
            gsub(".*: ", "\\1", .),
          status = gsub("Version Date.*", "\\1", info) %>%
            gsub(".*Version.: ", "\\1", .),
          date = gsub(".*Date: ", "\\1", info) %>%
            mdy())] %>%
  data.frame()
combined_scrape %>%
  head(2)
library(tidytext)

  rbindlist()
append(
big_list_node_td %>%
  head(),
big_list_node_text %>%
  map(str_count) %>%  head()
)
tmp[[1]][1] %>%
  str_count()

yolo = tmp %>%
  reduce(rbind)

runif(min = 1, max = length(big_list_node_text), n = 50)
index = sample(1:length(big_list_node_text), 50, replace = FALSE)


tibble(state = bill_data$state[index],
       bill_name = bill_data$bill_name[index],
       text = unlist(big_list_node_text[index])) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(state, bill_name, word, sort = TRUE) %>%
  bind_tf_idf(word, bill_name, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(state) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = state)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~state, ncol = 4, scales = "free") +
  labs(x = "tf-idf", y = NULL)


