#' Output bar plot of syllables and their frequencies
#' @param .data dataframe containing text and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper function
#' @param syll_col symbol name of insight column of interest
#' @param num_syllables number of syllables to visualise
#' @export
struct_syll_barplot <- function(.data, syll_col, num_syllables) {
    q_syll_col <- dplyr::enquo(syll_col)
    text_as_syll <- dplyr::pull(.data = .data, rlang::as_label(q_syll_col))
    allSyllables <- get_all_syllables_df(text_as_syll)

    ggplot2::ggplot(data = allSyllables[1:num_syllables,], aes(x = reorder(all_syllables, Freq), y = Freq)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(y = "Frequency", x = "Syllables") +
      ggplot2::coord_flip()
}