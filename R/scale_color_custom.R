#' Using your color palettes in ggplot
#'
#' scale_fill_custom is built on scale_fill_discrete, and can be added to a ggplot object to color tiles and bars appropriately
#' scale_color_custom is built on scale_color_discrete, and can be added to a ggplot object to color points and lines appropriately
#'
#' @details These functions will give errors if identifiers (ie taxa) are present in the data but not the palette (or if spelling and capitalized are not identical).
#' Use `palette_check()` to check whether everything lines up or to identify mismatches.
#' If you want to include some taxa (or other identifier) that are not in the palette of your choice, provide the new information in a "palette augment" dataframe using the optional `augment` argument.
#' `palette_augment_helper()` streamlines the development of your palette augment.
#'
#' Note that these functions are designed with a character vector in mind, rather than factors.
#' If you encounter errors, make sure the column of identifiers in your data is character form (`as.character()` may be useful here).
#'
#' @param dat.names vector of the taxa names or other identifiers from the data
#' @param palette.name Name of color palette used, defaults to "wolfe2014". See `make_colorvec.R` for details.
#' @param augment Optional data frame to account for taxa that are not in the palette.
#' One column is named "name" and contains taxa names (or whatever identifier is used).
#' Other column is named "color" and has the corresponding colors. `palette_augment_helper()` can assist in identifying missing colors and other issues.
#'
#' @return a layer for ggplot objects to specify the colors for "fill" aesthetics
#' (`scale_fill_custom()`) or "color" aesthetics (`scale_color_custom()`)
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' #generate simulated timeseries data
#' dat = as.data.frame(expand.grid(name = c("candida", "vibrio",
#'                                   "brachybacterium"), #species name
#'                                 day = c(1, 3, 7, 14, 21), #day of time series
#'                                 rep = 1:3,#replicate number
#'                                 stringsAsFactors = FALSE))
#' dat$count = 500 + #baseline
#'   100000 * (dat$day>1) +  #population generally gets near carrying capacity
#'   #  after first measurement
#'   (dat$day>1) * -20000*(dat$name == "candida") +#make candida CC lower
#'   (dat$day>1) *  30000*(dat$name == "vibrio") + #make vibrio CC higher
#'   (dat$day>1) * runif(nrow(dat), min = -20000, max = 20000)
#'   # add some random noise to observations except day 1
#' # check the palette
#' palette_check(dat$name, "wolfe2014")
#' #plot the timeseries
#' col.test = make_colorvec()
#' col.test = col.test[names(col.test) %in% dat$name]
#' ggplot(data = dat, aes(x = day,
#'                        y = count,
#'                        group = interaction(name, rep),
#'                        color = name))+
#'   geom_path(size = 0.8)+
#'   geom_point(size = 2)+
#'   scale_y_log10()+
#'   scale_color_custom(dat$name, "wolfe2014")+
#'   ggtitle("Simulated time series, 3 replicates per species")
#' ## now to aggregate + make barplot.
#' ## Here we'll imagine we want the mean abundance for each species broken
#' ## down by day
#' dat.summary = dat %>%
#'   group_by(name, day) %>%
#'   summarize(count = mean(count))
#' ggplot(data = dat.summary,
#'        aes(x = name, y = count, fill = name))+
#'   geom_col()+
#'   facet_wrap(~ day)+
#'   scale_fill_custom(dat.summary$name, "wolfe2014")+
#'   xlab("")+
#'   ggtitle("Simulated summary data, separated by day of experiment")


scale_color_custom = function(dat.names,
                             palette.name = "wolfe2014",
                             augment = NULL){
  stopifnot(is.character(dat.names))
  col.vec = make_colorvec()
  col.vec = col.vec[names(col.vec) %in% dat.names]
  if(!is.null(augment)){
    status.good = palette_augment_helper(dat.names = dat.names,
                                         palette.name = palette.name,
                                         augment = augment,
                                         spellcheck = FALSE,
                                         plot.palette = FALSE,
                                         verbose = FALSE
    )
    if(status.good){
      new.vec = augment$color
      names(new.vec) = augment$name
      col.vec = c(col.vec, new.vec)
    }else{
      cat("Augment fails check. Not adding until fixed")
    }
  }
  col.vec = col.vec[names(col.vec) %in% dat.names]
  scale_color_discrete(type = col.vec)
}
