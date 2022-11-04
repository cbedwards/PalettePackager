#' Assistance in adding a new palette to the package
#'
#' This function is based on the assumption that you'll develop/check a new palette using `palette_augment_helper()`.
#' Here the function gives instructions for how to convert your finalized augment dataframe into its own palette.
#' Note that this function does NOT change anything in the package, merely gives instructions on how to make those changes.
#'
#' @param augment The finalized augment dataframe, presumed to have been built iteratively with `palette_augment_helper()`.
#' Data frame with taxa (or other identifier names) in column "name" and colors in column "color"
#' @param palette.name.new Name for the new palette. Ensure this name is not already used for a different palette.
#'
#' @return Nothing; prints instructions to console.
#' @export
#'
#' @details
#' This package is written so that the main changes needed to include a new palette are
#' all in the function `make_colorvec()`. This includes adding in a hard-coded vector for the new palette,
#' updating the list of acceptable palettes within the function (`palette.name`) to make sure
#' that the new palette name gets through the error-checker, and updating the documentation of that file.
#'
#' I also recommend updating Readme.Rmd to describe the new palette and provide the color scheme
#' with `palette_vis()`. While not strictly necessary, this will help labmates and other package
#' users find the details without reading through help files.
#'
#' This package template was built using the tidyverse package development workflow;
#' adding to it or modifying it will be far easier if you also use that workflow. See [this chapter](https://r-pkgs.org/whole-game.html) for an overview.
#'
#' @examples
#' aug = data.frame(name = c("asclepias", "drosophila"),
#' color = c("coral", "purple"))
#' augment2palette(augment = aug,
#'                palette.name.new = "testPalette")
augment2palette = function(augment, palette.name.new){
  stopifnot(ncol(augment)==2,
            all(names(augment) %in% c("name","color")))
  col.vec = augment$name
  names(col.vec) = augment$name
  cat("Turning your augment into a package:",
      "Step 1: in R/make_colorvec.R, add a new `if()` statement after the existing ones. You can copy-paste the following:",
      "",
      paste0("if(palette.name == \"",palette.name.new,"\"){"),
      "col.vec = ",
      sep="\n")
  dput(col.vec)
  cat("}","", sep="\n")
  cat("(you may want to clean up the indenting slightly\n")
  cat("", "Step 2: still in make_colorvec.R, add the new palette name to palette.names (The first line within the function itself). If you don't do this, make_colorvec() will assume the new palette name is a mistake and won't let users give it in the palette.name argument.",
      "", "Step 3: still in make_colorvec.R, add the new palette name to the documentation in the line starting with #' @param",
      "Similarly, add a brief description of the palette (what it's intended use is, what if anything it's based on) under the @details header. Note that every line in this region should start with #'",
      "", "Step 4 (optional): add a description of what this palette is for in Readme.Rmd. You may want to add a chunk calling palette_vis() on the new palette (I find this visualization is a nice summary of the palette).",
      "", "Step 5: update the package. For details on managing packages, check out  https://r-pkgs.org/release.html",
      "If you have the appropriate packages (devtools, usethis installed and loaded (plus the separate software Rtools installed), you'll need to call the following:",
      "document()",
      "check()",
      "build_readme()",
      "install()",
      "",
      "If all those work successfully, all thet's left is to commit and push to your github repo (assuming you're using github to distribute this)",
      sep = "\n")

}
