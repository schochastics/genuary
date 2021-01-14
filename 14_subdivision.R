library(tidyverse)
subdiv_rect <- function(xmin,xmax,ymin,ymax,a,dir="h"){
  if(dir=="h"){
    ycut <- (1-a)*ymin+a*ymax
    return(tibble(xmin=c(xmin,xmin),xmax=c(xmax,xmax),
                  ymin=c(ymin,ycut),ymax=c(ycut,ymax)))
  } else{
    xcut <- (1-a)*xmin+a*xmax
    return(tibble(xmin=c(xmin,xcut),xmax=c(xcut,xmax),
                  ymin=c(ymin,ymin),ymax=c(ymax,ymax)))
  }
}

p <- runif(1,0.1,0.9)
start <- subdiv_rect(0,1,0,2,p,dir=sample(c("h","v"),1))
res <- start
res$recurse <- T
for(i in 1:12){
  tmp <- tibble()
  for(j in 1:nrow(res)){
    a <- res$xmin[j]
    b <- res$xmax[j]
    c <- res$ymin[j]
    d <- res$ymax[j]
    if(!res$recurse[j]){
      tmp <- bind_rows(tmp,tibble(xmin=a,xmax=b,ymin=c,ymax=d,recurse=FALSE))
    } else{
      ratio <- (b-a)/(d-c)
      if(ratio>1){
        dir <- "v"
      } else if(ratio<1){
        dir <- "h"
        
      } else{
        dir <- sample(c("h","v"),1)
      }
      p <- rnorm(1,0.5,0.15)  
      if(p<0.25){
        p <- 0.25
      } else if(p>0.75){
        p <- 0.75
      }
      recurse <- sample(c(FALSE,TRUE),2,replace = TRUE,prob=c(0.1,0.9))#prob=c((1-1/i)^0.15,1/i^0.15))
      tmp <- bind_rows(tmp,mutate(subdiv_rect(a,b,c,d,p,dir),recurse=recurse))
    }
  }
  res <- tmp
  if(all(!res$recurse)){
    break()
  }
}

cols <- c("#fff001","#ff0101","#0101fd","#f9f9f9")
res <- res %>% 
  mutate(fill=sample(cols,nrow(res),replace=TRUE,prob=c(0.15,0.15,0.15,0.55)))

ggplot(res,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax))+
  geom_rect(aes(fill=I(fill)),col="black")+
  theme_void()

ggsave("img/14_subdivision.png",width=5,height=5,dpi=300)
