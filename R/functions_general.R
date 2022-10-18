#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script holds functions for av legislation.
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

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

proces_av_leg_sum = function(file){
  # file = tar_read("av_leg_summary")

  av_leg_sum = read_xlsx(file, col_names = F) %>%
    setNames("text") %>%
    data.frame()

  av_leg_sum_pro = av_leg_sum %>%
    filter(!is.na(text)
           ,!str_detect(text, "History:")) %>%
    mutate(attribute = case_when(
      str_detect(text, "^Status:")~"status"
      ,str_detect(text, "^Date of Last Action:")~"last_action"
      ,str_detect(text, "^Author:")~"author"
      ,str_detect(text, "^Status:")~"status"
      ,str_detect(text, "^Topics:")~"topic"
      ,str_detect(text, "^Summary:")~"summary"
    )) %>%
    mutate(attribute = case_when(
      lead(attribute, 3) == "status" &
        lead(attribute, 4) == "last_action"~"bill_num"
      ,lead(attribute, 2) == "status" &
        lead(attribute, 3) == "last_action"~"year"
      ,lead(attribute, 1) == "status" &
        lead(attribute, 2) == "last_action"~"bill_name"
      ,T~attribute
    )) %>%
    mutate(text = gsub(".*:", "\\1", text) %>%
             str_remove_all("^\\*") %>%
             str_trim()
             ) %>%
    # filter(!is.na(attribute)) %>%
    mutate(bill_num = case_when(attribute == "bill_num"~text,T~ NA_character_)) %>%
    mutate(bill_num = case_when(bill_num == lag(bill_num, 8)~str_glue("{bill_num}_db"), T~bill_num)) %>%
    fill(bill_num) %>%
    filter(attribute != "bill_num") %>%
    mutate(index_bill = case_when(bill_num == lag(bill_num)~0, T~1) %>%  cumsum())

  av_leg_sum_pro  %>%
    pivot_wider(names_from = attribute
                ,values_from = text)  %>%
    mutate(state = str_trunc(bill_num, 2, 'right', ellipsis = "")
           ,year = as.numeric(year)
           ,status_short = gsub("-.*", "\\1", status))

}

#scraping functions============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

scrape_av = function(url){
  xml2::read_html(url)
}

safe_scrape_av = purrr::safely(scrape_av)

good_results_only = function(data){
  data %>%
    .["result"] %>%
    .[[1]]
}

safe_scrape_av_good = function(object){
  print("GO")
  object %>%
    .[[1]] %>%
    safe_scrape_av() %>%
    good_results_only()
}

extract_full_text = function(object){
  object %>%
    html_nodes(".text") %>%
    html_text()
}

extract_small_text = function(object){
  object %>%
    html_nodes(".code .indent") %>%
    html_text()
}

scrape_av_bill_legislation = function(){

  url = "https://www.ncsl.org/research/transportation/autonomous-vehicles-legislative-database.aspx"

  html <- read_html(url)

  object_xml = c(2022:2018) %>%
    paste0() %>%
    map(
      ~{
        result <- html_form(html) %>%
          pluck(1) %>%
          html_form_set(
            'dnn$ctr81355$StateNetDB$ckBxAllTopics' = "true",
            'dnn$ctr81355$StateNetDB$ckBxAllStates' = "true",
            'dnn$ctr81355$StateNetDB$ddlYear'       = .x) %>%
          html_form_submit("dnn$ctr81355$StateNetDB$btnSearch")

        result_formatted <- result %>%
          read_html() %>%
          rvest::html_nodes(".ModNCSLStateNetC div div+ div a")

        result_formatted_clean = result_formatted %>%
          html_attr("href") %>%
          reduce(rbind) %>%
          data.frame() %>%
          setNames("href") %>%
          mutate(bill_id = gsub(".*ID:bill:", "\\1", href) %>%
                   gsub('&ciq.*', "\\1", .) %>%
                   str_trim()) %>%
          mutate(state = str_trunc(bill_id, 2, "right", "")
                 ,year = str_sub(bill_id, 3) %>%
                   str_trunc(., 4, "right", "")
                 ,chamber = str_sub(bill_id, 3) %>%
                   str_remove_all("[:digit:]")
                 ,number = str_sub(bill_id, 3) %>%
                   str_remove_all(".*[:alpha:]")
                 ,bill_id_clean = str_glue("{state}_{chamber}_{number}")) %>%
          data.frame() %>%
          select(bill_id:bill_id_clean, everything()) %>%
          na.omit()
      }
    ) %>%
    reduce(bind_rows) %>%
    unique()

  full_result = object_xml %>%
    # sample_n(2) %>%
    group_by(bill_id, state, year, chamber, number, bill_id_clean) %>%
    nest() %>%
    mutate(scraped_xml = map(data
                             ,~{
                               Sys.sleep(1)
                               safe_scrape_av_good(.x)
                             })) %>%
    mutate(scraped_full_text = map(scraped_xml, extract_full_text)) %>%
    mutate(scraped_small_text = map(scraped_xml, extract_small_text))

  full_result %>%
    unnest(cols = c(scraped_full_text, scraped_small_text))

}


#data processing================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

process_summary_text = function(data
                                ,ngram_options = list(n = 2, n_min = 2, lowercase = T
                                                      ,stopwords = stop_words$word)){
  # data = tar_read("data_av_leg_sum")

  recipe = ngram_rec(data = data, ngram_options = ngram_options)

  data = recipe %>%  prep() %>%  juice() %>%
    pivot_longer(cols = starts_with("tf"))

  data
}

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  = tar_read("data_av_leg_sum")
#
# process = function(data){
#   # data = tar_read("data_av_leg_sum")
#
#   rec_tf = recipe(~ bill_num + summary, data = data) %>%
#     step_mutate(text = replace_contraction(summary)) %>%
#     step_mutate(text = lemmatize_strings(summary)) %>%
#     # step_stem(text) %>% #i dont like the results of this, may change
#     step_tokenize(summary,
#                   token = "words",
#                   options = list(lowercase = TRUE
#                                  ,strip_punct = TRUE
#                                  ,stopwords = stop_words$word
#                                  ,strip_numeric = TRUE
#                   )
#     ) %>%
#     step_tf(summary) #this step removes word sequence
#   # %>% step_nzv(all_predictors()) #this may remove a word that is in a bill many times but in no others, remove for now
#
#   data_rec_tf = rec_tf %>%
#     prep() %>%
#     juice() %>%
#     filter(!duplicated(bill_name))
#
#   data_rec_tf_long = data_rec_tf %>%
#     pivot_longer(cols = starts_with("tf_text")
#                  ,names_to = "text"
#                  ,values_to = "count") %>%
#     mutate(text = str_remove_all(text, "tf_text_"))
#
#
#   data_rec_tf_long_state = data_rec_tf_long %>%
#     group_by(state, text) %>%
#     summarise(count = sum(count), .groups = "drop")
#
#
#
#
#
#
#
#
# }
#
#


##logg odds work ==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pre_filter_data_LOGODDDS = function(data){
  # data = tar_read("data_av_leg_full")

  data %>%
    ungroup() %>%
    select(bill_id)



}

#script end=====================================================================










































