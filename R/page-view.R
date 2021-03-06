#' Colours a ggpage based on an insight function
#'
#' @param .data a dataframe containing "word" and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper
#'     function
#'
#' @param col_name symbol name of the insight column intended to
#'     colour plot
#'
#' @param num_terms the number of terms to visualise
#'
#' @param term_index which term to start the visualisation from
#'
#' @param palette determine coloration of palette (not yet implemented)
#'
#' @param word_size integer size of words 
#' 
#' @return ggplot object as per ggpage
#'
#' @export
struct_pageview <- function(.data, col_name, num_terms, term_index, word_size, palette){
    end <- min(nrow(.data), term_index + num_terms)
    q_col_name <- dplyr::enquo(col_name)

    if (rlang::as_label(q_col_name) %in% c("Term Sentiment", "Moving Average Term Sentiment", 
                        "Aggregated Sentiment")){
        # if afinn, bing, nrc, loughran (some numeric scale)
        if (sum(grepl("[[:digit:]]",.data[,rlang::as_label(q_col_name)])) > 0) {
            limit <- max(abs(.data[,rlang::as_label(q_col_name)]), na.rm = TRUE) * c(-1, 1) # limits for color palette
            .data[seq(term_index, end),] %>%
                dplyr::pull(text) %>%
                tidyr::replace_na(., list("[REMOVED]")) %>%
                unlist(.) %>%
                ggpage::ggpage_build() %>%
                dplyr::arrange(page, line) %>%
                dplyr::bind_cols(.data[seq(term_index, end),]) %>% 
                ggpage::ggpage_plot(ggplot2::aes(fill = !! q_col_name)) +
                ggplot2::scale_fill_distiller(palette = "RdYlGn", limit = limit, direction = 1) +
                ggplot2::geom_text(ggplot2::aes(label = text,
                                                x = (xmax + xmin)/2,
                                                y = (ymin + ymax)/2),
                                   size = word_size, color = "black")
        }
        # if nrc all senti, loughran all senti
        else {
            .data[seq(term_index, end),] %>%
                dplyr::pull(text) %>%
                tidyr::replace_na(., list("[REMOVED]")) %>%
                unlist(.) %>%
                ggpage::ggpage_build() %>%
                dplyr::arrange(page, line) %>%
                dplyr::bind_cols(.data[seq(term_index, end),]) %>% 
                ggpage::ggpage_plot(ggplot2::aes(fill = !! q_col_name)) +
                ggplot2::scale_fill_manual(values = c("grey70", "blue"),
                                  name = "Sentiment") +
                ggplot2::geom_text(ggplot2::aes(label = text,
                                                x = (xmax + xmin)/2,
                                                y = (ymin + ymax)/2),
                                   size = word_size, color = "black")
        }
    }
    else{
        limit <- c(0, max(.data[,rlang::as_label(q_col_name)], na.rm = TRUE)) # limits for color palette
        .data[seq(term_index, end),] %>%
            dplyr::pull(text) %>%
            tidyr::replace_na(., list("[REMOVED]")) %>%
            unlist(.) %>%
            ggpage::ggpage_build() %>%
            dplyr::arrange(page, line) %>%
            dplyr::bind_cols(.data[seq(term_index, end),]) %>% 
            ggpage::ggpage_plot(ggplot2::aes(fill = !! q_col_name)) +
            ggplot2::scale_fill_distiller(limit = limit, direction = 1) +
            ggplot2::geom_text(ggplot2::aes(label = text,
                                            x = (xmax + xmin)/2,
                                            y = (ymin + ymax)/2),
                               size = word_size, color = "black")
    }
}
