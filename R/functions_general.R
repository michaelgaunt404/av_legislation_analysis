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

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































