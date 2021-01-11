library(tidyverse)
df <- expand_grid(x=1:50,y=1:50)
df$type <- sample(c("l","r"),nrow(df),replace=TRUE)
mutate(df,xend=ifelse(type=="r",x+1,x),yend=y+1,x=ifelse(type=="l",x+1,x)) %>% 
  ggplot(aes(x,y,xend=xend,yend=yend))+geom_segment()+theme_void()+
  ggtitle("10 PRINT CHR$(205.5+RND(1)); : GOTO 10")

ggsave("img/05_golf.png",height = 5,width=5,dpi=300)
