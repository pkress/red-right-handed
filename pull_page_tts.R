##################)
### Author: Peter Kress
### Date: 9/13/22
### Purpose: Convert Red Hand Files to TTS
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
       , googleLanguageR)

## |- Handy Functions ----
`%p%` = paste0

month_diff = function(d1, d2){
  12*(year(d2) - year(d1)) + (month(d2) - month(d1))
}

make_title = function(x){ str_to_title(gsub("_", " ", x))}

## |- Set API key ----
gl_auth("./red_right_handed/gcp_tts_rrh_api_auth.json")

##################)
# | Read in Data ----
##################)

rrh_data =  fread("./red_right_handed/red_hand_files.csv")

##################)
# | Get Mp3 ----
##################)

make_mp3 = function(d){
  gl_talk(d)
}
gl_talk(rrh_data)
