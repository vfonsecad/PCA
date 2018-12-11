##########################################################

##### Visualize PCA

##########################################################

rm(list=ls())

library(plotly)
library(data.table)

setwd("D:\\Google Drive\\KUL PhD\\TeachingAssistant\\R\\PCA\\")

# Simulate data
X <- c(6, 1,2,3)
Y <- X+rnorm(4)
Z <- X+Y+rnorm(4)

Xorg <- data.table(X,Y,Z)
Xc <- data.table(scale(Xorg, center = T, scale = F))

ss <- svd(t(as.matrix(Xc))%*%as.matrix(Xc))
c1 <- ss$u[,1]
c2 <- ss$u[,2]
c3 <- ss$u[,3]

p <- plot_ly(Xc, x = ~X, y=~Y, z=~Z) %>% 
  add_markers(color=I("black")) %>%
  add_text(x=0, y=0,z=0, text="Origin",textfont = list(color = '#F00917', size = 16))
p

htmlwidgets::saveWidget(as_widget(p), "Data.html")

Comp1 <- 9*rbind(c1,-c1)
dim(Comp1)<-c(2,3)

p <- plot_ly(Xc, x = ~X, y=~Y, z=~Z) %>% 
  add_markers(color=I("black")) %>%
  add_trace(x = Comp1[,1], y = Comp1[,2], z = Comp1[,3],mode = "lines", type="scatter3d", color=I("blue"))%>%
  add_text(x = Comp1[1,1], y = Comp1[1,2],z =Comp1[1,3], text="t1",textfont = list(color = 'blue', size = 16))
p

htmlwidgets::saveWidget(as_widget(p), "PCA_1component.html")


Comp2 <- 9*rbind(c2,-c2)
p <- plot_ly(Xc, x = ~X, y=~Y, z=~Z) %>% 
  add_markers(color=I("black")) %>%
  add_trace(x = Comp1[,1], y= Comp1[,2], z= Comp1[,3],mode = "lines", type="scatter3d", color=I("blue"))%>%
  add_trace(x = Comp2[,1], y= Comp2[,2], z= Comp2[,3],mode = "lines", type="scatter3d", color=I("red"))%>%
  add_text(x = Comp1[1,1], y = Comp1[1,2],z =Comp1[1,3], text="t1",textfont = list(color = 'blue', size = 16))%>%
  add_text(x = Comp2[1,1], y = Comp2[1,2],z =Comp2[1,3], text="t2",textfont = list(color = 'red', size = 16))
p

htmlwidgets::saveWidget(as_widget(p), "PCA_2components.html")


Comp3 <- 9*rbind(c3,-c3)
p <- plot_ly(Xc, x = ~X, y=~Y, z=~Z) %>% 
  add_markers(color=I("black")) %>%
  add_trace(x = Comp1[,1], y= Comp1[,2], z= Comp1[,3],mode = "lines", type="scatter3d", color=I("blue"))%>%
  add_trace(x = Comp2[,1], y= Comp2[,2], z= Comp2[,3],mode = "lines", type="scatter3d", color=I("red"))%>%
  add_trace(x = Comp3[,1], y= Comp3[,2], z= Comp3[,3],mode = "lines", type="scatter3d", color=I("green"))%>%
  add_text(x = Comp1[1,1], y = Comp1[1,2],z =Comp1[1,3], text="t1",textfont = list(color = 'blue', size = 16))%>%
  add_text(x = Comp2[1,1], y = Comp2[1,2],z =Comp2[1,3], text="t2",textfont = list(color = 'red', size = 16))%>%
  add_text(x = Comp3[1,1], y = Comp3[1,2],z =Comp3[1,3], text="t3",textfont = list(color = 'green', size = 16))
p

htmlwidgets::saveWidget(as_widget(p), "PCA_3components.html")

t1 <- as.matrix(Xc)%*%c1
t2 <- as.matrix(Xc)%*%c2
t3 <- as.matrix(Xc)%*%c3

plot(t1,t2, xlim = c(-8,8), ylim=c(-8,8), col="black", pch=".", cex=10)
abline(h=0, col="blue")
abline(v=0, col="red")

plot(t2,t3, xlim = c(-8,8), ylim=c(-8,8), col="black", pch=".", cex=10)
abline(h=0, col="red")
abline(v=0, col="green")

plot(t1,t3, xlim = c(-8,8), ylim=c(-8,8), col="black", pch=".", cex=10)
abline(h=0, col="blue")
abline(v=0, col="green")

