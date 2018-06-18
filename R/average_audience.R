#' Average audience
#'
#' @param utilisateur User ID or screen name
#' @param token Your Twitter token
#' @return list followers
#' @export


audienceaveRage <- function(user,
                             token = NULL) {

  ctr <- as.numeric(length(user))

  while(ctr != 0) {
    user_temp <- user[ctr]
    user_info <- rtweet::lookup_users(user_temp, token = token)
    user_followers <- rtweet::get_followers(user_info$user_id, token = token)
    uf_details <- rtweet::lookup_users(user_followers$user_id, token = token)

    temp_moyenne_followers_compte <- uf_details %>%
      dplyr::mutate(sum_flw = sum(followers_count)) %>%
      dplyr::slice(1:1) %>%
      dplyr::select(sum_flw) %>%
      dplyr::mutate(moy_flw = sum_flw / as.numeric(length(uf_details$followers_count))) %>%
      dplyr:: mutate(screen_name = user[ctr])

    if(exists("iteration_users") == T) {
      iteration_users <- rbind(iteration_users, temp_moyenne_followers_compte) %>%
        dplyr::distinct(screen_name, .keep_all = TRUE) %>%
        dplyr::arrange(screen_name)

    }

    if(exists("iteration_users") == F) {

      iteration_users <- temp_moyenne_followers_compte

      rm(temp_moyenne_followers_compte)

    }

    ctr <- ctr - 1

  }

  iteration_users
}
