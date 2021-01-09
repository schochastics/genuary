library(tidyverse)
library(gganimate)
library(patchwork)
collatz <- function(n){
  if(n%%2==0){
    n/2
  } else{
    3*n+1
  }
}

angle_path <- function(theta, length = 1,
                       start = c(1, 1)) {
  
  theta_seq <- cumsum(theta)
  df <- rbind(start, length * data.frame(x = cos(theta_seq), y = sin(theta_seq)))
  df_path <- cumsum(df)
  df_path
}

collatz_path <- function(maxN = 500000000,samples=2000,a=0.11,e=1.3){
  colnums <- sample(1:maxN,samples)
  colList <- map(colnums,function(x){
    res <- x
    n <- x
    while(n!=1){
      n <- collatz(n)
      res <- c(res,n)
    }
    rev(res)
  })
  
  col_tbl <- map_dfr(colList,function(res){
    phi <- a*pi*(log(2)/log(6)-(res%%2))
    r <- res/(1+res^e)
    df <- angle_path(phi,r)
    thick <- 0.0001+0.1*((maxN-res[length(res)])/maxN)^(4*e)
    opac <- 0.05+0.5*(res[length(res)]/maxN)^3
    df$thick <- thick
    df$opac <- opac
    df$num <- 1:nrow(df)
    df
  },.id = "id")
  col_tbl  
}

col_tbl <- collatz_path()

p <- ggplot(col_tbl,aes(x,y,group=id))+
  geom_path(aes(alpha=opac,size=thick,col=num))+
  scale_alpha(range=c(0.2,0.6))+
  scale_size(range=c(0.2,1.4))+
  scale_color_gradientn(colours = cols)+
  theme_void()+
  theme(legend.position = "none")+
  transition_reveal(-num)

pa <- gganimate::animate(p,start_pause = 5,end_pause = 10)
anim_save("img/10_tree.gif",pa)


# simulation code
for(i in 1:250){
  a <- round(runif(1,0.01,0.75),2)
  e <- round(runif(1,1.0,1.5),2)
  col_tbl <- collatz_path(a=a,e=e)
  
  cols <- c("#006400", "#68228B", "#CD2626", "#EE7600", "#FFB90F")
  
  p <- ggplot(col_tbl,aes(x,y,group=id))+
    geom_path(aes(alpha=opac,size=thick,col=num))+
    scale_alpha(range=c(0.2,0.6))+
    scale_size(range=c(0.2,1.8))+
    scale_color_gradientn(colours = cols)+
    theme_void()+
    theme(legend.position = "none")
  out <- paste0("collatz/collatz_",str_pad(i,4,"left","0"),"-a_",a,"-e_",e,".png")
  ggsave(out,width = 10,height = 8)
}

# final image
a <- 0.12
e <- 1.24
tree1 <- collatz_path(a=a,e=e)
p1 <- ggplot(tree1,aes(y,x,group=id))+
  geom_path(aes(alpha=I(5*opac),size=I(5*thick)),col="white")+
  ggraph::theme_graph(background = "black")+
  theme(legend.position = "none")

a <- 0.19
e <- 1.38
tree2 <- collatz_path(a=a,e=e)
p2 <- ggplot(tree2,aes(y,x,group=id))+
  geom_path(aes(alpha=I(5*opac),size=I(5*thick)),col="black")+
  ggraph::theme_graph(background = "white")+
  theme(legend.position = "none")


a <- 0.22
e <- 1.27
tree3 <- collatz_path(a=a,e=e)
cols <- c("#9b5de5","#f15bb5","#fee440","#00bbf9","#00f5d4")
p3 <- ggplot(tree3,aes(x,y,group=id))+
  geom_path(aes(alpha=I(5*opac),size=I(5*thick),col=log(num)))+
  scale_color_gradientn(colours = cols)+
  ggraph::theme_graph(background = "grey25")+
  theme(legend.position = "none")


a <- 0.16
e <- 1.18
tree4 <- collatz_path(a=a,e=e)
p4 <- ggplot(tree4,aes(x,y,group=id))+
  geom_path(aes(alpha=I(5*opac),size=I(5*thick),col=sqrt(num)))+
  scale_color_gradientn(colours = cols)+
  ggraph::theme_graph(background = "black")+
  theme(legend.position = "none")

p4

pfull <- p1+p2+p3+p4+plot_layout(ncol=2,nrow=2)
ggsave("img/10_trees.png",pfull,width = 10,height = 10,dpi = 72)
