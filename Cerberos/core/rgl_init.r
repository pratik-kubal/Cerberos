rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
        if( new.device | rgl.cur() == 0 ) {
          rgl.open()
          par3d(windowRect = 50 + c( 0, 0, width, width ) )
          rgl.bg(color = bg )
        }
        rgl.clear(type = c("shapes", "bboxdeco"))
        rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
      }
