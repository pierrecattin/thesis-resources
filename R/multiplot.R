#' Multiple plot function
#' Source: http://www.guru-gis.net/multiplot-function-for-ggplot/
#'
#' @param ... plots
#' @param plotlist list of plots
#' @param cols number of plots in one line
#' @param layout If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.
#' @param title main title
#'
#' @return ggplot
#' @export
#'
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title="") {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (nchar(title)>0){
    layout<-rbind(rep(0, ncol(layout)), layout)
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), heights =if(nchar(title)>0){unit(c(0.5, rep(5,nrow(layout)-1)), "null")}else{unit(c(rep(5, nrow(layout))), "null")} )))

    # Make each plot, in the correct location
    if (nchar(title)>0){
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp=gpar(fontsize=16))
    }

    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
