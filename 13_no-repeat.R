library(tidyverse)
library(Polychrome)
library(gganimate)
fun <- function(x){
  n <- c()
  i <- 2
  r <- x
  while(prod(n)!=x){
    if(!r%%i) {n=c(n,i);r=r/i;i=1}
    i <- i+1
  }
  if(x==1){
    n <- 1
  }
  n
}

mnum <- 1023

prfac_tbl <- tibble(number=1:mnum) %>% 
  rowwise() %>% 
  mutate(prfac=list(fun(number))) %>% 
  ungroup() %>% 
  mutate(factors=map_int(prfac,length)) %>% 
  mutate(is_prime = factors==1) 

# prime_cols <- palette36.colors(n=28)[-c(1,2)]
primes <- which(prfac_tbl[["factors"]]==1)
prime_cols <- createPalette(length(primes),c("#EF6F6D","#51B8B1","#E5DC81"))
names(prime_cols) <- primes

x <- 0
y <- 0
dx <- 1
dy <- 0
n <- 1
i <- 1

res <- tibble(n=1:mnum,x=0,y=0)
for(i in 1:mnum){
  if ( n * n + 1 == i ) {
    dy = (n %% 2)*2-1;
    dx = 0;
    n <- n+1
  } else if ( n * n - n + 1 == i) {
    dx = (n %% 2)*2-1;
    dy = 0;
  }
  x <- x + dx
  y <- y + dy
  res$x[i] <- x
  res$y[i] <- y
}

dat <- prfac_tbl %>% 
  left_join(res,by=c("number"="n")) %>% 
  rename(xmid=x,ymid=y) %>% 
  unnest(prfac) %>% 
  group_by(number) %>% 
  mutate(x=1/factors,x=cumsum(x)) %>% unnest(prfac) %>% 
  mutate(alpha = ifelse(is_prime,1,0.5)) %>% 
  mutate(col=prime_cols[match(prfac,names(prime_cols))])

res <- bind_rows(tibble(n=0,x=0,y=0),res)
p <- ggplot()+
  geom_rect(data=dat,aes(xmin=xmid-0.5+x-1/factors,xmax=xmid-0.5+x,fill=I(col),alpha=I(alpha),
                ymin=ymid-0.5,ymax=ymid+0.5),col="black",size=0.15)+
  # geom_tile(data=res,aes(x,y),size=0.45,fill="NA",col="black")+
  annotate("text",x=0,y=0,label="0",size=2)+
  coord_fixed()+
  theme_void()

ggsave("img/13_no-repeat.png",p,width=5,height=5,dpi=300)

pa <- p+transition_time(number)+shadow_mark()
pani <- animate(pa,nframes=125,fps=10,end_pause=25)
gganimate::save_animation(pani,"img/13_no-repeat.gif")
