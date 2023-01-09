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

make_audio = function(d, outdir){
  intro = paste(paste0("Issue number ", d$issue_num
                               , " published on ", d$issue_month)
                , paste0("Written by ", gsub(", (?=[A-Z']+$)", ", and "
                                             , gsub("; ", ", ", d$from_names)
                                             , perl = T))
                , paste0("Questions: "
                         , gsub("\n\nNext Question\n\n"
                                , " \n\n Next Question. \n\n"
                                , d$questions))
                , sep = ".\n")
  gl_talk(intro, outdir%p%"/intro.mp3", audioEncoding = 'MP3'
          , name = "en-GB-Wavenet-D", speakingRate = 1, pitch = -2)
  
}

make_answers = function(d, outdir){
  response = "Responses from Nick. \n\n"%p%d$text
  resp_len = nchar(response)
  rounds = ceiling(resp_len/5000L)
  lapply(1:rounds, \(x){
    gl_talk(substr(response, 5000 * (x-1) + 1, 5000*x)
            , outdir%p%"/response_"%p%x%p%".mp3", audioEncoding = 'MP3'
            , name = "en-GB-Wavenet-D", speakingRate = 1, pitch = -2)
    })
  
}

make_mp3 = function(d){
  outdir = paste0("./red_right_handed/", d$issue_num, "_"
                  , gsub("https://www.theredhandfiles.com/", "", d$title, fixed = T))
  
  dir.create(outdir)
  
  make_intro(d, outdir)
  make_answers(d, outdir)
  
  system(paste0("cat ", outdir, "/*.mp3 > ", outdir, "/full.mp3"))
  
}

lapply(1:11, \(x){make_mp3(rrh_data[x])})
