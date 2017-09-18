#Replace x,y, z as input$var when building interface

library(rgl)

x_plot <- feature1
y_plot <- datavar$Incidences
z_plot <- feature2
grid.lines =26
x.pred <- seq(min(x_plot), max(x_plot), length.out = grid.lines)
z.pred <- seq(min(z_plot), max(z_plot), length.out = grid.lines)
xz <- expand.grid( x_plot = x.pred, z_plot = z.pred)

ones <- as.matrix(rep.int(1,nrow(xz)))
xz_l <- as.matrix(cbind(ones,xz))
xz_q <- polyMat(xz_l)
y.pred_q <- matrix(profX(xz_q,theta_q,x_q),nrow = grid.lines,ncol = grid.lines)

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}
rgl_init()
aspect3d(1,1,1)
plot3d(x_plot,y_plot,z_plot,size = 0.5,type = 's',xlab = "People under Poverty in Rural Areas in Millions",ylab = "Incidences of Homicide",zlab = "People under Poverty in Urban Areas in Millions",col = "#000000",box=FALSE,axes =TRUE, main="Comparison")
#text3d(x_plot,y_plot,z_plot,texts = datavar$States,adj = 0.5,col="red")

rgl.surface(x.pred, z.pred, y.pred_q, color = "steelblue", 
            alpha = 0.5, lit = FALSE)  
rgl.surface(x.pred, z.pred, y.pred_q, color = "black",
            alpha = 0.5, lit = FALSE, front = "lines", back = "lines")

y.pred_l <- matrix(profX(xz_l,theta_l,x),nrow = grid.lines,ncol = grid.lines)
rgl.surface(x.pred, z.pred, y.pred_l, color = "green", 
            alpha = 0.5, lit = FALSE)  
rgl.surface(x.pred, z.pred, y.pred_l, color = "black",
            alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
detach(datavar)
rgl.close()
