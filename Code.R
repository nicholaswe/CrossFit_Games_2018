# USUAL PACKAGES ----------------------------------------------------------

library(RJSONIO)
library(tidyverse)
library(glue)
library(httr)
library(xml2)
library(rvest)
library(magrittr)
library(gsubfn)
library(stringr)
library(lubridate)
library(highcharter)
library(xlsx)

# OPTIONS -----------------------------------------------------------------

options(digits.secs = 3) # Shows miliseconds

# JSON DATA ---------------------------------------------------------------

url <- fromJSON("https://games.crossfit.com/competitions/api/v1/competitions/games/2018/leaderboards?division=1&sort=0&page=1")

# DIRTY DATA --------------------------------------------------------------------

data <- url[['leaderboardRows']]

# CLEAN DATA --------------------------------------------------------------

Games2018 <- data.frame(Name=sapply(data, function(x) x$entrant$competitorName) %>% unlist() %>% as.character(),
                        ID=sapply(data, function(x) x$entrant$competitorId),
                        Box=sapply(data, function(x) {ifelse (class(x$entrant$affiliateName)=="NULL", "-", x$entrant$affiliateName)}),
                        Country=sapply(data, function(x) x$entrant$countryCode),
                        W1_clas=sapply(data, function(x) x$scores[[1]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W1_points=sapply(data, function(x) x$scores[[1]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W1_time=sapply(data, function(x) x$scores[[1]]$time), # Formato character
                        W1_time2=sapply(data, function(x) x$scores[[1]]$time) %>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W2_clas=sapply(data, function(x) x$scores[[2]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W2_points=sapply(data, function(x) x$scores[[2]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W2_time=sapply(data, function(x) x$scores[[2]]$time), # Formato character
                        W2_time2=sapply(data, function(x) x$scores[[2]]$time) %>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W3_clas=sapply(data, function(x) x$scores[[3]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W3_points=sapply(data, function(x) x$scores[[3]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W3_weight=sapply(data, function(x) x$scores[[3]]$scoreDisplay) %>% gsub("lb", "", .) %>% as.numeric(), 
                        W4_clas=sapply(data, function(x) x$scores[[4]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W4_points=sapply(data, function(x) x$scores[[4]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W4_time=sapply(data, function(x) x$scores[[4]]$time), # Formato character
                        W4_time2=sapply(data, function(x) x$scores[[4]]$time) %>% as.POSIXct(., format="%H:%M:%OS"), #  POSIXct Format
                        W5_clas=sapply(data, function(x) x$scores[[5]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W5_points=sapply(data, function(x) x$scores[[5]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W5_time=sapply(data, function(x) x$scores[[5]]$time), # Formato character
                        W5_time2=sapply(data, function(x) x$scores[[5]]$time)%>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W5_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[5]]$time))==TRUE, "11:00.00", sapply(data, function(x) x$scores[[5]]$time)), # time sem CAP, formato character
                        W5_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[5]]$time))==TRUE, "11:00.00", sapply(data, function(x) x$scores[[5]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        W6_clas=sapply(data, function(x) x$scores[[6]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W6_points=sapply(data, function(x) x$scores[[6]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W7_clas=sapply(data, function(x) x$scores[[7]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W7_points=sapply(data, function(x) x$scores[[7]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W7_time=sapply(data, function(x) x$scores[[7]]$time), # Formato character
                        W7_time2=sapply(data, function(x) x$scores[[7]]$time)%>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W7_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[7]]$time))==TRUE, "6:00.00", sapply(data, function(x) x$scores[[7]]$time)), # time sem CAP, formato character
                        W7_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[7]]$time))==TRUE, "6:00.00", sapply(data, function(x) x$scores[[7]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        W8_clas=sapply(data, function(x) x$scores[[8]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W8_points=sapply(data, function(x) x$scores[[8]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W8_time=sapply(data, function(x) x$scores[[8]]$time), # Formato character
                        W8_time2=sapply(data, function(x) x$scores[[8]]$time)%>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W8_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[8]]$time))==TRUE, "40:00.00", sapply(data, function(x) x$scores[[8]]$time)), # time sem CAP, formato character
                        W8_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[8]]$time))==TRUE, "40:00.00", sapply(data, function(x) x$scores[[8]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        W9_clas=sapply(data, function(x) x$scores[[9]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W9_points=sapply(data, function(x) x$scores[[9]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W9_time=sapply(data, function(x) x$scores[[9]]$time), # Formato character
                        W9_time2=sapply(data, function(x) x$scores[[9]]$time)%>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W9_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[9]]$time))==TRUE, "12:00.00", sapply(data, function(x) x$scores[[9]]$time)), # time sem CAP, formato character
                        W9_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[9]]$time))==TRUE, "12:00.00", sapply(data, function(x) x$scores[[9]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        W10_clas=sapply(data, function(x) x$scores[[10]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W10_points=sapply(data, function(x) x$scores[[10]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W10_time=sapply(data, function(x) x$scores[[10]]$time), # Formato character
                        W10_time2=sapply(data, function(x) x$scores[[10]]$time) %>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W11_clas=sapply(data, function(x) x$scores[[11]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W11_points=sapply(data, function(x) x$scores[[11]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W11_time=sapply(data, function(x) x$scores[[11]]$time), # Formato character
                        W11_time2=sapply(data, function(x) x$scores[[11]]$time) %>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W11_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[11]]$time))==TRUE, "6:00.00", sapply(data, function(x) x$scores[[11]]$time)), # time sem CAP, formato character
                        W11_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[11]]$time))==TRUE, "6:00.00", sapply(data, function(x) x$scores[[11]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        W12_clas=sapply(data, function(x) x$scores[[12]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W12_points=sapply(data, function(x) x$scores[[12]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W12_time=sapply(data, function(x) x$scores[[12]]$time), # Formato character
                        W12_time2=sapply(data, function(x) x$scores[[12]]$time) %>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W13_clas=sapply(data, function(x) x$scores[[13]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W13_points=sapply(data, function(x) x$scores[[13]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W13_time=sapply(data, function(x) x$scores[[13]]$time), # Formato character
                        W13_time2=sapply(data, function(x) x$scores[[13]]$time)%>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W13_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[13]]$time))==TRUE, "4:00.00", sapply(data, function(x) x$scores[[13]]$time)), # time sem CAP, formato character
                        W13_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[13]]$time))==TRUE, "4:00.00", sapply(data, function(x) x$scores[[13]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        W14_clas=sapply(data, function(x) x$scores[[14]]$rank) %>% gsub("T", "", .) %>% as.numeric(),
                        W14_points=sapply(data, function(x) x$scores[[14]]$breakdown) %>% gsub("pts", "", .) %>% as.numeric(),
                        W14_time=sapply(data, function(x) x$scores[[14]]$time), # Formato character
                        W14_time2=sapply(data, function(x) x$scores[[14]]$time)%>% as.POSIXct(., format="%M:%OS"), #  POSIXct Format
                        W14_time3=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[14]]$time))==TRUE, "8:00.00", sapply(data, function(x) x$scores[[14]]$time)), # time sem CAP, formato character
                        W14_time4=ifelse(grepl("CAP", sapply(data, function(x) x$scores[[14]]$time))==TRUE, "8:00.00", sapply(data, function(x) x$scores[[14]]$time)) %>% as.POSIXct(., format="%M:%OS"), # time sem CAP,  POSIXct Format
                        Rank=sapply(data, function(x) x$overallRank) %>% as.numeric())

Games2018 <- Games2018 %>% mutate_if(is.factor, as.character) # All factor variables to character


# RE-SCALE FUNCTION -------------------------------------------------------

resc <- function(a, b, x){ # a = lower re-scale bound, b= upper re-scale bound
  
  ind <- 0
  
    if(class(x)=="character"){ # Verifying if is a time or rep variable
    
    ind <- 1  
    x <- x %>% hms() %>% period_to_seconds() # Transforming time in seconds
    
  }
  
  min <- min(x, na.rm=TRUE) # To ignore "NA" charactere cases
  max <- max(x, na.rm=TRUE)
  
  out <- sapply(x, function(y) (((b-a)*(y-min)/(max-min))+a)) # Re-scale function
  
  if(ind==1){
    out <- b+a-out
  }
  
  return(out)
  
}

# RE-SCALED DATA ----------------------------------------------------------

dataResc <- Games2018 %>% 
  select(Name, Box, Country, paste0("W",1:14,"_clas"), paste0("W",c(1,2,4,5,7,8,9,10,11,12,13,14), "_time"), paste0("W",c(5, 7, 8, 9, 11, 13, 14), "_time3"), W3_weight) %>% 
  mutate(W1_timeResc = resc(1,5,W1_time)) %>% 
  mutate(W2_timeResc = resc(1,5,W2_time)) %>% 
  mutate(W3_weightResc = resc(1,5,W3_weight)) %>% 
  mutate(W4_timeResc = resc(1,5,W4_time)) %>% 
  mutate(W5_timeResc = resc(1,5,W5_time3)) %>% 
  mutate(W6_clasResc = (resc(1,5,W6_clas)*(-1)+5+1)) %>% # Different from others, in this variable, the smaller the better
  mutate(W7_timeResc = resc(1,5,W7_time3)) %>% 
  mutate(W8_timeResc = resc(1,5,W8_time3)) %>% 
  mutate(W9_timeResc = resc(1,5,W9_time3)) %>% 
  mutate(W10_timeResc = resc(1,5,W10_time)) %>% 
  mutate(W11_timeResc = resc(1,5,W11_time3)) %>% 
  mutate(W12_timeResc = resc(1,5,W12_time)) %>%
  mutate(W13_timeResc = resc(1,5,W13_time3)) %>% 
  mutate(W14_timeResc = resc(1,5,W14_time3))


# RADAR CHART -------------------------------------------------------------

radarchart <- highchart() %>% # Cria o ambiente do gráfico
  hc_chart(polar = TRUE, type = "line") %>% # Gráfico de radar
  hc_title(text = "<b>CrossFit Games 2018</b>") %>%  # Título
  hc_legend(enabled = F) %>% 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0,
           labels=list(enabled=F)) %>% 
  hc_xAxis(labels=list(style= list("color"= 'black', "fontSize" = '20px')),
           categories = c("CRIT",	"30 MUSCLE-UPS",	"CROSSFIT TOTAL",	"MARATHON ROW",	"THE BATTLEGROUND",	"CLEAN & JERK SPEED LADDER",
                          "FIBONACCI",	"MADISON TRIPLUS",	"CHAOS",	"BICOUPLET 2",	"BICOUPLET 1",	"TWO-STROKE PULL",
                          "HANDSTAND WALK",	"AENEAS"),
           tickmarkPlacement = "on",
           lineWidth = 0) %>% 
  hc_add_series( name=dataResc$Name[1], Clasi=dataResc[1,4:17] %>% unlist() %>% as.vector(),Tempo=dataResc[1,c(18:19,37,20:21,9,22:29)] %>% unlist() %>% as.vector(),
                 data = dataResc[1,38:51] %>% unlist() %>% as.vector(),Box=dataResc$Box[1]) %>% 
  hc_add_series( name=dataResc$Name[2],Clasi=dataResc[2,4:17] %>% unlist() %>% as.vector(),Tempo=dataResc[2,c(18:19,37,20:21,9,22:29)] %>% unlist() %>% as.vector(),
                 data = dataResc[2,38:51] %>% unlist() %>% as.vector(),Box=dataResc$Box[2]) %>% 
  hc_add_series( name=dataResc$Name[3],Clasi=dataResc[3,4:17] %>% unlist() %>% as.vector(),Tempo=dataResc[3,c(18:19,37,20:21,9,22:29)] %>% unlist() %>% as.vector(),
                 data = dataResc[3,38:51] %>% unlist() %>% as.vector(),Box=dataResc$Box[3]) %>% 
  # hc_add_series( name=dataResc$Nome[4],Clasi=dataResc[4,4:17] %>% unlist() %>% as.vector(),Tempo=dataResc[4,c(18:19,37,20:21,9,22:29)] %>% unlist() %>% as.vector(),
  #                data = dataResc[4,38:51] %>% unlist() %>% as.vector(),Box=dataResc$Box[4]) %>%
  hc_add_series( name=dataResc$Name[38],Clasi=dataResc[38,4:17] %>% unlist() %>% as.vector(),Tempo=dataResc[38,c(18:19,37,20:21,9,22:29)] %>% unlist() %>% as.vector(),
                 data = dataResc[38,38:51] %>% unlist() %>% as.vector(),Box=dataResc$Box[38]) %>%
  hc_tooltip(useHTML=T,formatter = JS("function(){
                                      window.x=this;
                                      return ('<table><tr><td    style=\"text-align: right\"><span style=\"color:' + this.series.color + '\"> Athlete</span>:</td><td>' + this.series.name + 
                                      '</td></tr><tr><td style=\"text-align: right\"><span style=\"color:' + this.series.color + '\"> Box</span>:</td><td>' + this.series.userOptions.Box +
                                      '</td></tr><tr><td style=\"text-align: right\"><span style=\"color:' + this.series.color + '\"> Classification</span>:</td><td>' + this.series.userOptions.Clasi[this.point.index]+ 
                                      'º</td></tr><tr><td style=\"text-align: right\"><span style=\"color:' + this.series.color + '\"> Time/Reps</span>:</td><td>' + this.series.userOptions.Tempo[this.point.index]+'</td></tr></table>')
                                      }"))

radarchart

