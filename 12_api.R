library(tidyverse)
library(magick)
library(gganimate)

pic <- magick::image_read(jsonlite::fromJSON("https://random.dog/woof.json")$url)
pic <- image_scale(pic,"200x")
pic1 <- image_data(pic,"rgb")
pic1 <- as.numeric(pic1)
pic1 <- apply(pic1,1,rgb)
img_tbl <- tibble(x=rep(1:nrow(pic1),ncol(pic1)),y=rep(ncol(pic1):1,each=nrow(pic1)),col=c(pic1)) 

img_tbl <- img_tbl %>% 
  mutate(xnew=dense_rank(col)) %>%
  mutate(xnew=xnew/max(xnew)*max(x)) %>% 
  group_by(col) %>% 
  mutate(ynew=row_number()) %>% 
  ungroup() %>% 
  mutate(ynew=ynew/max(ynew)*max(y))

df <- tibble(x=c(img_tbl$x,img_tbl$xnew),
       y=c(img_tbl$y,img_tbl$ynew),
       id=rep(1:nrow(img_tbl),2),
       col=rep(img_tbl$col,2),
       time=rep(c(1,10),each=nrow(img_tbl)))

p <- ggplot(df,aes(x,y))+
  geom_point(aes(col=I(col)))+
  theme_void()+
  transition_time(time)

pa <- animate(p,100,fps=25,start_pause = 25,end_pause = 25)
gganimate::save_animation(pa,"img/12_api.gif")


