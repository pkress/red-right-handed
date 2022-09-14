##################)
### Author: Peter Kress
### Date: 9/13/22
### Purpose: Convert red hand files to audio files
##################)

##################)
# | Initialize Workspace ----
##################)
## |- Paths ----
setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))
setwd("..")


## |- Packages ----

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , rvest)

## |- Handy Functions ----
`%p%` = paste0

month_diff = function(d1, d2){
  12*(year(d2) - year(d1)) + (month(d2) - month(d1))
}

make_title = function(x){ str_to_title(gsub("_", " ", x))}

##################)
# | Find pages  ----
##################)

find_pages = function(page_num){
  pages_url = base_url%p%"page/"%p%page_num
  
  page_list = rvest::read_html(pages_url) %>% 
    html_nodes(".posts__article-title") %>% 
    html_children() %>% 
    html_attr("href") %>% 
    gsub(".{0,}com/|/", "", .)
  
}

base_url = "https://www.theredhandfiles.com/"

page_nums = 1:7


all_pages = lapply(page_nums, find_pages) %>% 
  unlist()

##################)
# | Pull Page Text ----
##################)

pull_page = function(page_url){
  #' Title
  #'
  #' @param url 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  cat("pulling info for page:", page_url, "\n\n")
  
  ## Pull page html
  page_html = read_html(page_url)
  
  ## Extract title info
  title_block = page_html %>% 
    html_elements(".post__title-block") %>% 
    html_children() %>% 
    html_text2() %>% 
    trimws() %>% 
    .[.!=""]
  
  ## Basic info
  issue_num = title_block[1] %>% 
    str_extract(".{0,} /") %>% 
    str_extract("\\d") %>% 
    as.numeric()
  
  issue_month = title_block[1] %>% 
    str_extract("/ .{0,}$") %>% 
    str_sub(3,-1)
  
  ## Questions
  question_cnt = (length(title_block)-1)/2
  questions = title_block[(1:question_cnt)*2] %>% 
    paste(collapse = "\n\nNext Question\n\n")
  
  ## Writer information
  get_from = function(from_info){
    if(length(from_info)==3){
      from_names = from_info[1]
      from_cities = from_info[2]
      from_countries = from_info[3]
    } else if (length(from_info)==2){
      warning("Strange from format for: "%p%page_url)
      cat("Strange from format for: "%p%page_url)
      from_names = from_info[1]
      from_cities = from_info[2]
      from_countries = from_info[2]
    } else {
      warning("Very strange from format for: "%p%page_url)
      cat("\nVery strange from format for: "%p%page_url)
      from_names = ""
      from_cities = ""
      from_countries = ""
    }
    return(list(from_names = from_names, from_cities = from_cities
                , from_countries = from_countries))
  }
  if(length(title_block)>=3){
    from_info = title_block[(1:question_cnt)*2+1] %>% 
      str_split(", ", n = 3) 
    out_from_info = lapply(from_info, get_from)
    from_names = lapply(out_from_info, `[[`, "from_names") %>% 
      unlist() %>% 
      paste(collapse = "; ")
    from_cities = lapply(out_from_info, `[[`, "from_cities") %>% 
      unlist() %>% 
      paste(collapse = "; ")
    from_countries = lapply(out_from_info, `[[`, "from_countries") %>% 
      unlist() %>% 
      paste(collapse = "; ")
  } else if(length(title_block==2)) {
    from_names = "" 
    from_cities = ""
    from_countries = ""
  } else { 
    stop("Strange title info")
  }
  
  ## Extract text
  text = page_html %>% 
    html_element(".article") %>% 
    html_children() %>% 
    .[2] %>% 
    html_elements("p") %>% 
    html_text2() %>% 
    .[1:(length(.)-1)] %>% 
    paste(collapse = "\n\n ") %>% 
    trimws()
  
  ## extract sidebar image
  sidebar = page_html %>% 
    html_element(".article__sidebar") 
  
  image_caption = sidebar %>% 
    html_text2()
  
  image_url = sidebar %>% 
    html_children() %>% 
    .[[1]] %>% 
    html_children() %>% 
    html_attr("src")
  
  ## output
  output = list(issue_num = issue_num, issue_month = issue_month
                , url = page_url, title = gsub(base_url, "", page_url)
                , question_cnt = question_cnt
                , from_names = from_names, from_cities = from_cities
                , from_countries = from_countries
                , questions = questions, text = text
                , image_url = image_url, image_caption = image_caption)
  return(output)
}

out_pages = lapply(base_url%p%all_pages, pull_page) %>% 
  rbindlist()

fwrite(out_pages, "Documents/personal_projects/red_right_handed/red_hand_files.csv")