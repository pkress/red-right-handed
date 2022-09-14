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
       , RSelenium, rvest)

## |- Handy Functions ----
`%p%` = paste0

month_diff = function(d1, d2){
  12*(year(d2) - year(d1)) + (month(d2) - month(d1))
}

make_title = function(x){ str_to_title(gsub("_", " ", x))}

## |- Set API key ----

tts_api = Sys.getenv("tts_api")

##################)
# | Read in Data ----
##################)

rrh_data =  fread("Documents/personal_projects/red_right_handed/red_hand_files.csv")

##################)
# | Get Mp3 ----
##################)

aa = rvest::session("https://ttstool.com/")

### Best option is probably free tier for Polly from AWS

base_url = "https://api.voicerss.org/"
params = c(hl = "en-us", key = tts_api, c = "MP3", r = "2", v = "john"
           , f = "48khz_16bit_stereo")
payload = rrh_data[1, text]
download_url = paste0(base_url, "?"
                      , paste(names(params)%p%"="%p%params, collapse = "&")
                      , "&src=", payload)
download.file("https://api.voicerss.org/?key=d5d2a220d838435c99bc4edf48bdccba&hl=en-us&c=MP3&r=2&v=john&f=48khz_16bit_stereo&src=Hello,%20world!
  
 ")
rD <- RSelenium::rsDriver(
  port = 4444L,
  browser = c("firefox"),
  version = "latest"
)
