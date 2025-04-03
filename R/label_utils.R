
#' @title Plot a single label with a central barcode and various fields of information to be printed on the label
#' @param layout a list of rectangle coordinates corresponding to boxes harbouring different label's parts
#' @param texts a vector of fields text values information, same length as layout
#' @param labels a vector of fields label, same length as layout
#' @param fontface a vector of fontfaces, same length as layout
#' @param fontsize a vector of fontsizes, same length as layout
#' @param bc a single text value that will be used for the barcode
#' @param bctype the type of barcode (either '128' or 'qc')
#' @param inner.border should borders of individual rectangles be drawn (TRUE/FALSE)
#'
#' @export
make_single_label <- function(layout,
                               texts,
                               labels,
                               fontface,
                               fontsize,
                               bc="", 
                               bctype=c("qr","128"),
                               inner.border=TRUE)
{
  if (!all(c(length(layout)==length(texts),
             length(layout)==length(labels),
             length(layout)==length(fontface),
             length(layout)==length(fontsize)))){
    stop("layout,texts,labels,fontface,fontsize should have the same length")
  }
  if (inner.border){
    lapply(layout, function(a){
      grid::grid.rect(x = grid::unit(a[[1]][1], "npc"),
                      y = grid::unit(a[[1]][2], "npc"),
                      width = grid::unit(a[[2]][1]-a[[1]][1], "npc"), 
                      height = grid::unit(a[[2]][2]-a[[1]][2], "npc"),
                      hjust=0, vjust = 0, gp = gpar(lex=0.1))
    })
  }
    lapply(seq_along(layout), function(a){
      yt <- layout[[a]][[1]][2] + (layout[[a]][[2]][2]-layout[[a]][[1]][2])/3
      yl <- yt + (layout[[a]][[2]][2]-layout[[a]][[1]][2])/3
      xt <- layout[[a]][[1]][1] + (layout[[a]][[2]][1]-layout[[a]][[1]][1])/2
      grid::grid.text(label = texts[a],
                      x = grid::unit(xt, "npc"),
                      y = grid::unit(yt, "npc"), 
                      gp = gpar( fontsize=fontsize[a], fontface=fontface[a]), hjust=0.5, vjust=0)
      grid::grid.text(label = labels[a],
                      x = grid::unit(xt, "npc"),
                      y = grid::unit(yl, "npc"), 
                      gp = gpar( fontsize=fontsize[a]), hjust=0.5, vjust=0)
    })

  if (bc!=""){
    if (bctype=="qr"){
      grid::pushViewport(viewport(x = grid::unit(0.5, "npc"),
                                  y = grid::unit(0.5, "npc"),
                                  width = grid::unit(0.33, "npc"),
                                  height = grid::unit(0.33, "npc"),
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



#' @title Print labels as a pdf file 
#' @param data a data.frame with one row per label
#' @param filename output filename
#' @param layout the single label layout (see make_single_label function)
#' @param columns the names of the columns in data to be used (same length as layout)
#' @param bccol the name of the column to use for the barecode
#' @param bctype the type of barcode (either '128' or 'qc') 
#' @param label_sizes the description of label format
#' @param byrow TRUE or FALSE. In case of label pages should the page be filled by row or by column
#' @param fontface font face (bold or plain)
#' @param fontsize font size
#' @param inner.border draw border inside label to separate fields
#' @param outer.border draw rectangle around label
#'
#' @export
print_labels <- function(data, filename, layout, columns, bccol, bctype = '128', label_sizes, byrow=TRUE, fontface="normal", fontsize=6 , inner.border=TRUE, outer.border=TRUE){
  data <- as.data.table(data)
  page_wdt <- label_sizes$pg.W.in
  page_hgt <- label_sizes$pg.H.in
  numrow <- label_sizes$nRows.pg
  numcol <- label_sizes$nColumns.pg
  wdt <- label_sizes$Lab.W.in
  hgt <- label_sizes$Lab.H.in
  top_mar = label_sizes$Height.margin.in
  bot_mar = label_sizes$Height.margin.in
  left_mar = label_sizes$Width.margin.in
  right_mar = label_sizes$Width.margin.in
  row_space <- (page_hgt - top_mar - bot_mar - numrow*hgt)/(numrow-1)
  col_space <- (page_wdt - left_mar - right_mar - numcol*wdt)/(numcol-1)
  family <- "sans"
  if (byrow==TRUE){
    data <- data.table(setnames(expand.grid(1:numcol,1:numrow)[,c(2,1)],c("lrow","lcol")), data)
  } else {
    data <- data.table(setnames(expand.grid(1:numrow,1:numcol),c("lrow","lcol")), data)
  }
  data[,N:=1:.N]
  cols <- match(columns, colnames(data))
  bccol <- match(bccol, colnames(data))
  whichn <- which(colnames(data)=="N") 
  n <- length(cols)
  if (length(fontsize)==1) fontsize <- rep(fontsize, n)
  if (length(fontface)==1) fontface <- rep(fontface, n)
  
  grDevices::pdf(filename,
                 width = page_wdt, 
                 height = page_hgt,
                 onefile = TRUE, compress = FALSE,
                 family = family) # Letter size paper from Avery
  
  # Grid layout for labels
  label_layout <- grid::grid.layout(numrow, numcol,
                                    widths = grid::unit(c(rep(wdt + col_space, numcol-1), wdt), "in"),
                                    heights = grid::unit(c(rep(hgt + row_space, numrow-1), hgt), "in"))
  
  grid::pushViewport(grid::viewport(layout = label_layout))
  apply(data,1,function(a) {
    if ((as.numeric(a[whichn])-1)%%(numrow*numcol)==0 & as.numeric(a[whichn])>1) {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = label_layout))
    }
    grid::pushViewport(viewport(layout.pos.row=a[1],
                                layout.pos.col=a[2]))
    grid::pushViewport(viewport(x = grid::unit(0, "npc"),
                                y = grid::unit(1, "npc"),
                                width = grid::unit(wdt, "in"),
                                height = grid::unit(hgt, "in"),
                                just = c('left','top')))
    if (outer.border){
      if (label_sizes$Rounded){
        grid.draw(roundrectGrob(gp=gpar(lex=0.1)))
      } else {
        grid.draw(rectGrob(gp=gpar(lex=0.1)))
      }
    }
    make_single_label(layout = layout, texts = a[cols], labels = colnames(data)[cols], bc = a[bccol], bctype = bctype, fontsize = fontsize, fontface = fontface, inner.border = inner.border)
    
    popViewport(1)
    popViewport(1)
    
  })
  dev.off()
  
}