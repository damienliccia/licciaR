#' Average audience
#'
#' @param utilisateur User ID or screen name
#' @param token Your Twitter token
#' @return list followers
#' @export
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe

user_in_depth <- function(user,
                          token = NULL,
                          n_tweets = NULL,
                          output = NULL) {

  ctr <- as.numeric(length(user))

  user_temp <- user[ctr]
  user_info <- rtweet::lookup_users(user_temp, token = token)
  user_timeline <- rtweet::get_timeline(user_info$user_id, token = token, n = n_tweets)

  list_possible_outputs <- dplyr::data_frame(outputs = c("top_users_rt",
                                                  "words_most_used"),
                                      n = c("1",
                                            "2"))

  list_possible_outputs <- list_possible_outputs %>%
    dplyr::filter(stringr::str_detect(outputs, output))

  if(list_possible_outputs$n == 1) {

    # Top users rt
    top_users_rt <- user_timeline %>%
      dplyr::filter(stringr::str_detect(text, "rt @") == T) %>%
      dplyr::mutate(text = stringr::str_to_lower(text)) %>%
      dplyr::select(text) %>%
      dplyr::mutate(users_most_rt = gsub('^.*rt @\\s*|\\s*:.*$', "", text)) %>%
      dplyr::count(users_most_rt, sort = T) %>%
      dplyr::mutate(screen_name = user_timeline$screen_name[1]) %>%
      dplyr::rename(n_top_users_rt = n) %>%
      dplyr::select(screen_name,
                    users_most_rt,
                    n_top_users_rt)

  } else  if(list_possible_outputs$n == 2) {

    # Words most used
    words_most_used <- user_timeline %>%
      dplyr::filter(stringr::str_detect(text, "rt @") == F) %>%
      dplyr::mutate(text = stringr::str_to_lower(text)) %>%
      edouaRd::wordfrequency(text) %>%
      dplyr::mutate(screen_name = user_timeline$screen_name[1]) %>%
      dplyr::rename(n_words = n) %>%
      dplyr::select(screen_name,
                    words,
                    n_words)
  } else {
    print("erreur")
  }
}

