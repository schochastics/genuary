library(tidyverse)
library(gganimate)

lissajou <- function(a,b,delta){
  t <- seq(0,2*pi,0.01)
  x <- sin(a*t+delta)
  y <- sin(b*t)
  tibble(x,y)
}


n <- 7
params <- tibble(a=sample(1:10,n,replace = FALSE),
                 b=sample(2:10,n,replace = FALSE)+0.5,
                 delta=pi/sample(2:10,n,replace=FALSE))

params <- params %>% 
  arrange(a)
res <- params %>% 
  rowwise() %>% 
  mutate(xy=list(lissajou(a,b,delta)))

xy_list <- res$xy
k <- 0
df <- tibble()
dfx <- tibble()
dfy <- tibble()
for(i in 1:n){
  tmpx <- tibble(x = xy_list[[i]]$x+i*2.5,time = seq(0,2*pi,0.01),y=n*2.5+1.5,yend=1)
  tmpy <- tibble(y = xy_list[[i]]$y+i*2.5,time = seq(0,2*pi,0.01),x=n*2.5+1.5,xend=1)
  dfx <- bind_rows(dfx,tmpx)
  dfy <- bind_rows(dfy,tmpy)
  for(j in 1:n){
    k <- k+1
    tmp <- tibble(x=xy_list[[i]]$x,y=xy_list[[j]]$y,grp=k,time=seq(0,2*pi,0.01))
    tmp$x <- tmp$x+i*2.5
    tmp$y <- tmp$y+j*2.5
    df <- bind_rows(df,tmp)
  }
}

#static plot of last frame
df %>% 
  ggplot()+
  geom_path(aes(x,y,group=grp,col=as.factor(grp)))+
  theme_void()+
  theme(legend.position = "none",panel.background = element_rect(fill="black"))

ggsave("img/08_curves.png",width = 5,height = 5,dpi = 300)

df1 <- df %>% 
  group_by(grp) %>% 
  mutate(xend=lag(x),yend=lag(y)) %>% 
  ungroup()


ggplot(df1)+
  geom_segment(data=dfx,aes(x,y,xend=x,yend=yend),col="grey",linetype="dotted",size=0.2)+
  geom_segment(data=dfy,aes(x,y,xend=xend,yend=y),col="grey",linetype="dotted",size=0.2)+
  geom_segment(aes(x,y,xend=xend,yend=yend,col=as.factor(grp)))+
  geom_point(aes(x,y),col="white")+
  theme_void()+
  theme(legend.position = "none",panel.background = element_rect(fill="black"))+
  transition_time(time)+
  shadow_mark(exclude_layer = c(1,2,4)) -> p

pa <- animate(p,nframes = 200,fps=15,end_pause = 5)
anim_save("img/08_curves.gif",pa)
