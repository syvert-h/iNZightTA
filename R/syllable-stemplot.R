#' Output ggplot filled with stemplots of the ith_syllable(s)
#' @param .data a dataframe containing text and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper function
#' @param col_name symbol name of column insight outputted to
#' @param num_stem_plots number of stem plots to produce from slider
#' @export
struct_syll_stemplot <- function(.data, col_name, num_stem_plots) {
  q_col_name <- dplyr::enquo(col_name)
  hyphened_text <- dplyr::pull(.data = .data, rlang::as_label(q_col_name))
  all_syllables_df <- get_all_syllables_df(hyphened_text)
  unique_hyphened_text <- unique(hyphened_text)
  lst_ith_syllable_df <- lapply(num_stem_plots[1]:num_stem_plots[2], get_ith_syll_df, all_syllables_df, hyphened_text) #list of each syllable as data frame

  plots <- lapply(lst_ith_syllable_df, ith_syll_stemplot) #list of stemplots for each syllable
  library(gridExtra)
  gridExtra::grid.arrange(gridExtra::arrangeGrob(grobs = plots))
}


#' Create stemplot for ith syllable
#' @param ith_syllable_df data frame of the ith syllable and its prefixes/suffixes
#' @return stemplot for ith syllable (ggplot object)
#' @export
ith_syll_stemplot <- function(ith_syllable_df) {
  library(ggfittext)
  library(randomcoloR)
  text_colour <- randomcoloR::randomColor(1) #random colour for each stemplot
  intervals <- round(1/nrow(ith_syllable_df), 5)
  if (intervals < 0.1) {
    intervals <- 0.1 # 10 labels (rows) maximum
    ith_syllable_df <- ith_syllable_df[1:10,] #already sorted ordered most to least common
  }
  ymin <- seq(from = 0, by = intervals, along.with = ith_syllable_df$before)
  ymax <- seq(from = intervals, by = intervals, along.with = ith_syllable_df$before)
  ggplot2::ggplot(ith_syllable_df) +
    ggfittext::geom_fit_text(aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, label = toupper(before)), grow = T, place = "right", show.legend = F, colour = text_colour) +
    ggfittext::geom_fit_text(aes(xmin = 1, xmax = 2, ymin = 0, ymax = max(ymax), label = toupper(syllable[1]), angle = 90), grow = T, show.legend = F, colour = text_colour) +
    ggfittext::geom_fit_text(aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, label = toupper(after)), grow = T, place = "left", show.legend = F, colour = text_colour) +
    coord_fixed() +
    theme_void()
}
