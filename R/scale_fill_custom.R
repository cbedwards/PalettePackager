#' @inherit scale_color_custom
#' @export

scale_fill_custom = function(dat.names,
                            palette.name = "wolfe2014",
                            augment = NULL){
  stopifnot(is.character(dat.names))
  col.vec = make_colorvec()
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
  scale_fill_discrete(type = col.vec)
}

