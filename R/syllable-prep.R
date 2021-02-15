#' Attempt to split text by its syllables using hyphenation
#' @param .data character vector of terms
#' @return character vector of text split into syllables (insight column)
#' @export
get_text_as_syllable <- function(.data) {
  library(sylly.en)
  .data[is.na(.data)] <- " " # remove NA values
  hyphened_text_df <- sylly::hyphen(.data, hyph.pattern = "en", as = "data.frame")
  hyphened_text_df$word
}


#' Generate data frame of all syllables and their frequencies found in text_as_syllables
#' @param hyphened_text text_as_syllables insight column
#' @return data frame with all syllables and their frequencies
#' @export
get_all_syllables_df <- function(hyphened_words) {
  all_syllables <- unlist(strsplit(hyphened_words, "-"))
  all_syllables <- all_syllables[all_syllables != " "] #removes " " which were formerly NA values
  all_syll_to_df <- data.frame(table(all_syllables))
  all_syll_df <- all_syll_to_df[order(all_syll_to_df$Freq, decreasing = T),] #ordered most to less frequent
  all_syll_df$all_syllables <- as.character(all_syll_df$all_syllables) #convert factors to character values
  all_syll_df
}


#' Generates data frame for ith syllable containing the syllable and all prefix/suffix occurrences of the 
#' syllable in the text
#' @param i number specifying which syllable to look at
#' @param syll_df data frame containing all syllables and their frequency
#' @param text_hyphened text_as_syllables insight column
#' @return data frame containing ith syllable and all prefixes/suffixes to the syllable
#' @export
get_ith_syll_df <- function(i, syll_df, text_hyphened) {
  syll_i <- syll_df$all_syllables[i]
  syll_pattern <- paste0("-", syll_i, "$", "|", "-", syll_i, "-", "|", "^", syll_i, "-", "|", "^", syll_i, "$")
  occurrences <- text_hyphened[grep(syll_pattern, text_hyphened)]
  temp_df <- data.frame(table(occurrences))
  temp_df <- temp_df[order(temp_df$Freq, decreasing = T),] #order rows by text_hyphened frequency
  temp_df$occurrences <- as.character(temp_df$occurrences) #convert factors to character types
  occurrences_split <- strsplit(temp_df$occurrences, syll_pattern)
  lengths <- sapply(occurrences_split, length)
  occurrences_split[lengths == 1] <- lapply(occurrences_split[lengths == 1], append, "") #some occurrences missing suffix value
  syll_i_df <- do.call(rbind.data.frame, occurrences_split)
  syll_i_df <- cbind(syll_i, syll_i_df)
  colnames(syll_i_df) <- c("syllable", "before", "after")
  syll_i_df
}