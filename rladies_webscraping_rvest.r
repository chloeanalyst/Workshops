#rladies-dublin
### https://rud.is/b/2017/09/19/pirating-web-content-responsibly-with-r/
### https://dmi3kno.github.io/polite/
### https://web.archive.org/web/20190414202617/http://blog.mischel.com/2011/12/20/writing-a-web-crawler-politeness/

install.packages('rvest')
library(rvest)
library(tidyverse)
library(robotstxt) #Checks the file on website to see if you can legally scrape
library(janitor)
library(tidytext)
library(purr)


#---- Data Mining in R

#Check the t&c to see if you are allowed to scrape data from the website. (Conditions of use / Terms of use)

#ropensci.org/blog


#-------- EXAMPLE 1 - Scraping titles

#--- Legal check - robotstxt

paths_allowed("https://ropensci.org/blog/")

get_robotstxt("https://ropensci.org/blog/")

#--- Scrape 1st Page - rvest 
#--- Selector Gadget Google Chrome

title_1 <- read_html("https://ropensci.org/blog/")

title_1 <- title_1 %>% html_nodes(".post-title") %>%
  html_text(trim = TRUE)


#--- Scrape multiple pages
#--- Create a function to scrape titles of any page

get_title <- function(page_number) {
  
  Sys.sleep(2)
  
  link <- paste0("https://ropensci.org/blog/page/", page_number)
  
  read_html(link) %>% 
    html_nodes(".post-title") %>%
    html_text(trim = TRUE)
  
    }

get_title(58)


#---Now we can iterate (Populates the page ranges you'd like to scrape)

map(1:58, get_title) %>%
  unlist()

#-- Run in small iterations to allow memory on laptop

title_2_30 <- map(2:30, get_title) %>% unlist()

title_31_58 <- map(31:58, get_title) %>% unlist()

#Currently a vector
ropensci_titles <- c(title_1,title_2_30,title_31_58)

#Convert to a tibble/df and find out what the most common words used removing stop_words and numbers

tibble(title = ropensci_titles) %>%
  unnest_tokens(output = word, input = title, strip_numeric = TRUE) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stop_words)


#-------- EXAMPLE 2 Scraping tables

# Check Legal

get_robotstxt("https://en.wikipedia.org/wiki/List_of_best-selling_music_artists")

paths_allowed("https://en.wikipedia.org/wiki/List_of_best-selling_music_artists")


charts_html <- read_html("https://en.wikipedia.org/wiki/List_of_best-selling_music_artists") 

charts_list <- charts_html %>% 
  html_nodes(".wikitable") %>%
  html_table()

charts <- tibble(charts_list)

#Check the results - duplicate cols due to misspelling
unnest(data = charts, cols = charts_list) %>% view()

#clean the names (Remove string from `artist`) and save
charts <- unnest(data = charts, cols = charts_list) %>% view() %>% 
  clean_names

#clean_names removes the dash but appends with a value eg _2
names(charts)

#merge multiple cols, if has NA in one col it will replace with the other col then remove duplicates

charts <- charts %>% 
  mutate(release_first_charted = coalesce(release_year_of_first_charted_record, release_year_of_first_charted_record_2), .after = period_active) %>% 
  select(-release_year_of_first_charted_record,-release_year_of_first_charted_record_2)



charts <- rename(charts, total_certified_units = total_certified_units_from_available_markets_a)

## Regex for cleaning data 
# Cheat sheet - https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# remove > [10]
# \\[\\d*+\\]

charts <- charts %>% 
  mutate(release_first_charted = as.numeric(str_remove_all(release_first_charted, "\\[\\d*+\\]"))) %>%
  mutate(period_active = str_remove_all(period_active, "\\[\\d*+\\]"))


## Fixing Claimed Sales

charts <- charts %>% 
  separate(claimed_sales,
           c("claimed_sales","claimed_sales_alternative"),sep = "\\[\\d*+\\]", extra = "merge")


#clean the alt col

charts <- charts %>% mutate(claimed_sales_alternative = str_remove_all(claimed_sales_alternative,"\\[\\d*+\\]"))

