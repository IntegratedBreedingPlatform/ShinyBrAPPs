#' Create a 4 rows x 2 columns label with 8 potential fields + 1 bare/qrcode
#'
#' @param texts 
#' @param labels 
#' @param fontface 
#' @param fontsize 
#' @param bc 
#' @param bctype 
#'
#' @returns
#' @export
#' @import grid
#' @examples
make_single_label <- function(texts=rep("",8),
                              labels = rep("",8),
                              fontface = rep(2,8),
                              fontsize = rep(4,8),
                              bc="", 
                              bctype=c("qr","128"))
{
  texts <- paste(labels,ifelse(labels=="","",": "),texts,sep="")
  grid::grid.text(label = texts[1],
                  x = grid::unit(0.05, "npc"),
                  y = grid::unit(0.8, "npc"), 
                  gp = gpar(fontface=fontface[1], fontsize=fontsize[1]), hjust = 0, vjust=0)
  grid::grid.text(label = texts[2],
                  x = grid::unit(0.95, "npc"),
                  y = grid::unit(0.8, "npc"), 
                  gp = gpar(fontface=fontface[2], fontsize=fontsize[2]), hjust = 1, vjust=0)
  grid::grid.text(label = texts[3],
                  x = grid::unit(0.05, "npc"),
                  y = grid::unit(0.7, "npc"), 
                  gp = gpar(fontface=fontface[3], fontsize=fontsize[3]), hjust = 0, vjust=0)
  grid::grid.text(label = texts[4],
                  x = grid::unit(0.95, "npc"),
                  y = grid::unit(0.7, "npc"), 
                  gp = gpar(fontface=fontface[4], fontsize=fontsize[4]), hjust = 1, vjust=0)
  grid::grid.text(label = texts[5],
                  x = grid::unit(0.05, "npc"),
                  y = grid::unit(0.3, "npc"), 
                  gp = gpar(fontface=fontface[5], fontsize=fontsize[5]), hjust = 0, vjust=1)
  grid::grid.text(label = texts[6],
                  x = grid::unit(0.95, "npc"),
                  y = grid::unit(0.3, "npc"), 
                  gp = gpar(fontface=fontface[6], fontsize=fontsize[6]), hjust = 1, vjust=1)
  grid::grid.text(label = texts[7],
                  x = grid::unit(0.05, "npc"),
                  y = grid::unit(0.2, "npc"), 
                  gp = gpar(fontface=fontface[7], fontsize=fontsize[7]), hjust = 0, vjust=1)
  grid::grid.text(label = texts[8],
                  x = grid::unit(0.95, "npc"),
                  y = grid::unit(0.2, "npc"), 
                  gp = gpar(fontface=fontface[8], fontsize=fontsize[8]), hjust = 1, vjust=1)
  if (bc!=""){
    if (bctype=="qr"){
      grid::pushViewport(viewport(x = grid::unit(0.5, "npc"),
                                  y = grid::unit(0.5, "npc"),
                                  width = grid::unit(0.4, "npc"),
                                  height = grid::unit(0.4, "npc"),
                                  just = c('centre','centre')))
      
    } else {
      grid::pushViewport(viewport(x = grid::unit(0.5, "npc"),
                                  y = grid::unit(0.5, "npc"),
                                  width = grid::unit(0.6, "npc"),
                                  height = grid::unit(0.2, "npc"),
                                  just = c('centre','centre')))
      
    }
    grid::grid.draw(make_qrcode(bc, type=bctype))
    popViewport(1)    
  }
  
}

#' Generate a QRcode or a code128 barcode from a single ID
#'
#' @param my_id 
#' @param ec_level 
#' @param type 
#' 
#' @returns a rasterGrob object
#' @export
#' @import raster
#' @import grid
#' @import qrencoder
#' @import baRcodeR
#' @examples
make_qrcode <- function (id, ec_level = 3, type="qr") 
{
  if (type=="qr"){
    aa <- raster::raster(qrencoder::qrencode_raw(as.character(id), ec_level))
    qr <- grid::rasterGrob(raster::as.raster(aa,col = c("white", "black")), interpolate = FALSE)
  } else {
    qr <- baRcodeR:::code_128_make(id)
    
  }
  return(qr)
} 


#' Create a 4 rows x 2 columns label with 8 potential fields + 1 bare/qrcode
#'
#' @param texts 
#' @param labels 
#' @param fontface 
#' @param fontsize 
#' @param bc 
#' @param bctype 
#'
#' @returns
#' @export
#' @import grid
#' @examples
make_single_label2 <- function(texts=rep("",8),
                              labels = rep("",8),
                              fontface = rep(2,8),
                              fontsize = rep(4,8),
                              bc="", 
                              bctype=c("qr","128"))
{
  #texts <- paste(labels,ifelse(labels=="","",": "),texts,sep="")
  grid::grid.segments(x0 = unit(0,"npc"),
                      x1 = unit(1,"npc"),
                      y0 = unit(0.2,"npc"),
                      y1 = unit(0.2,"npc"),
                      gp = gpar(lex=0.1))
  grid::grid.segments(x0 = unit(0,"npc"),
                      x1 = unit(1,"npc"),
                      y0 = unit(0.4,"npc"),
                      y1 = unit(0.4,"npc"),
                      gp = gpar(lex=0.1))
  grid::grid.segments(x0 = unit(0,"npc"),
                      x1 = unit(1,"npc"),
                      y0 = unit(0.6,"npc"),
                      y1 = unit(0.6,"npc"),
                      gp = gpar(lex=0.1))
  grid::grid.segments(x0 = unit(0,"npc"),
                      x1 = unit(1,"npc"),
                      y0 = unit(0.8,"npc"),
                      y1 = unit(0.8,"npc"),
                      gp = gpar(lex=0.1))
  
  grid::grid.segments(x0 = unit(0.5,"npc"),
                      x1 = unit(0.5,"npc"),
                      y0 = unit(0,"npc"),
                      y1 = unit(0.4,"npc"),
                      gp = gpar(lex=0.1))
  grid::grid.segments(x0 = unit(0.5,"npc"),
                      x1 = unit(0.5,"npc"),
                      y0 = unit(0.6,"npc"),
                      y1 = unit(1,"npc"),
                      gp = gpar(lex=0.1))
  
  grid::grid.text(label = texts[1],
                  x = grid::unit(0.25, "npc"),
                  y = grid::unit(0.85, "npc"), 
                  gp = gpar(fontface=fontface[1], fontsize=fontsize[1]), just="center", vjust=0)
  grid::grid.text(label = texts[2],
                  x = grid::unit(0.75, "npc"),
                  y = grid::unit(0.85, "npc"), 
                  gp = gpar(fontface=fontface[2], fontsize=fontsize[2]), just="center", vjust=0)
  grid::grid.text(label = texts[3],
                  x = grid::unit(0.25, "npc"),
                  y = grid::unit(0.65, "npc"), 
                  gp = gpar(fontface=fontface[3], fontsize=fontsize[3]), just="center", vjust=0)
  grid::grid.text(label = texts[4],
                  x = grid::unit(0.75, "npc"),
                  y = grid::unit(0.65, "npc"), 
                  gp = gpar(fontface=fontface[4], fontsize=fontsize[4]), just="center", vjust=0)
  grid::grid.text(label = texts[5],
                  x = grid::unit(0.25, "npc"),
                  y = grid::unit(0.25, "npc"), 
                  gp = gpar(fontface=fontface[5], fontsize=fontsize[5]), just="center", vjust=0)
  grid::grid.text(label = texts[6],
                  x = grid::unit(0.75, "npc"),
                  y = grid::unit(0.25, "npc"), 
                  gp = gpar(fontface=fontface[6], fontsize=fontsize[6]), just="center", vjust=0)
  grid::grid.text(label = texts[7],
                  x = grid::unit(0.25, "npc"),
                  y = grid::unit(0.05, "npc"), 
                  gp = gpar(fontface=fontface[7], fontsize=fontsize[7]), just="center", vjust=0)
  grid::grid.text(label = texts[8],
                  x = grid::unit(0.75, "npc"),
                  y = grid::unit(0.05, "npc"), 
                  gp = gpar(fontface=fontface[8], fontsize=fontsize[8]), just="center", vjust=0)
  
  grid::grid.text(label = labels[1],
                  x = grid::unit(0.01, "npc"),
                  y = grid::unit(0.95, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=0, vjust=0)
  grid::grid.text(label = labels[2],
                  x = grid::unit(0.99, "npc"),
                  y = grid::unit(0.95, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=1, vjust=0)
  grid::grid.text(label = labels[3],
                  x = grid::unit(0.01, "npc"),
                  y = grid::unit(0.75, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=0, vjust=0)
  grid::grid.text(label = labels[4],
                  x = grid::unit(0.99, "npc"),
                  y = grid::unit(0.75, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=1, vjust=0)
  grid::grid.text(label = labels[5],
                  x = grid::unit(0.01, "npc"),
                  y = grid::unit(0.35, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=0, vjust=0)
  grid::grid.text(label = labels[6],
                  x = grid::unit(0.99, "npc"),
                  y = grid::unit(0.35, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=1, vjust=0)
  grid::grid.text(label = labels[7],
                  x = grid::unit(0.01, "npc"),
                  y = grid::unit(0.15, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=0, vjust=0)
  grid::grid.text(label = labels[8],
                  x = grid::unit(0.99, "npc"),
                  y = grid::unit(0.15, "npc"), 
                  gp = gpar(fontface="plain", fontsize=3), hjust=1, vjust=0)
  
  if (bc!=""){
    if (bctype=="qr"){
      grid::pushViewport(viewport(x = grid::unit(0.5, "npc"),
                                  y = grid::unit(0.5, "npc"),
                                  width = grid::unit(0.4, "npc"),
                                  height = grid::unit(0.4, "npc"),
                                  just = c('centre','centre')))
      
    } else {
      grid::pushViewport(viewport(x = grid::unit(0.5, "npc"),
                                  y = grid::unit(0.5, "npc"),
                                  width = grid::unit(0.6, "npc"),
                                  height = grid::unit(0.2, "npc"),
                                  just = c('centre','centre')))
      
    }
    grid::grid.draw(make_qrcode(bc, type=bctype))
    popViewport(1)    
  }
  
}
