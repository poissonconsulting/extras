# # Test gamma count deviance
# library(dplyr)
# library(tidyr)
# library(ggplot2)
#
# # When you pick nice numbers, the alpha < 0.5 rule works
# # pars_theta <- c(seq(0.1, 1.1, by = 0.05), seq(2, 20, by = 0.5))
# # pars_lambda <- seq(1, 10000, by = 100)
# #
# # pars <- expand_grid(lambda = pars_lambda, theta = pars_theta)
# # pars$fail <- NA
#
# # But when you pick stranger values, it doesn't :(
# pars2_theta <- rlnorm(10, 1, 1)
# pars2_lambda <- rlnorm(10, 2, 1)
#
# pars2 <- expand_grid(lambda = pars2_lambda, theta = pars2_theta)
# pars2$fail_x <- NA
# pars2$fail_mean <- NA
# pars2$diff_x <- NA
# pars2$diff_mean <- NA
# pars2$dev <- NA
#
# for (i in 1:nrow(pars2)) {
#   # print(paste("i = ", i, ", theta is ", pull(pars2[i, "theta"]), ", lambda is ", pull(pars2[i, "lambda"])))
#   n <- 1000
#   lambda <- pull(pars2[i, "lambda"])
#   theta <- pull(pars2[i, "theta"])
#   x <- ran_upois(n, lambda, theta)
#
#   test <-
#     tibble(
#       x = x, lambda = lambda, theta = theta
#     ) %>%
#     mutate(
#       ll = log_lik_upois(x, lambda, theta)
#     )
#
#   test$sat_x = log_lik_upois(test$x, pmax(test$x - (test$theta / (1 + test$theta)), 0), test$theta)
#   # test$sat_mean = log_lik_upois(test$mean, test$lambda, test$theta)
#   test$dev <- dev_upois(test$x, test$lambda, test$theta, res = TRUE)
#
#   # chk_false(any(test$sat < test$ll))
#   pars2$fail_x[i] <- vld_true(any(test$sat_x < test$ll))
#   # pars2$fail_mean[i] <- vld_true(any(test$sat_mean < test$ll))
#
#   diff <- test$sat_x[test$sat_x < test$ll] - test$ll[test$sat_x < test$ll]
#   pars2$diff_x[i] <- mean(diff)
#   pars2$dev[i] <- any(dev < 0)
#   # diff <- test$sat_mean[test$sat_mean < test$ll] - test$ll[test$sat_mean < test$ll]
#   # pars2$diff_mean[i] <- mean(diff)
#
#
#   # opt_gamma_count <- function(alpha, x, lambda) {
#   #   -log_lik_gamma_count(x = x, lambda = lambda, alpha = alpha)
#   # }
#   # if (length(lambda) == 1) {lambda <- rep(lambda, length(x))}
#   # if (length(alpha) == 1) {alpha <- rep(alpha, length(x))}
#   # opt_alpha <- rep(NA, length(x))
#   # bol <- !is.na(x) & !is.na(lambda) & !is.na(alpha)
#   # for (i in seq_along(x)) {
#   #   if (bol[i] & !is.na(bol[i])) {
#   #     opt_alpha[i] <- stats::optimize(
#   #       opt_gamma_count,
#   #       interval = c(0, 1e10),
#   #       x = x[i],
#   #       lambda = lambda[i]
#   #     )$minimum
#   #   }
#   # }
#
#   # test$dev <- dev_gamma_count(x, lambda, alpha)
#   # test$sat <- test$dev / 2 + test$ll
#
#   # test$ll_pois <- log_lik_pois(test$x, test$lambda)
#   # test$sat_pois <- log_lik_pois(test$x, test$x)
#
#   # test$ll_pois <- log_lik_pois(test$x, test$lambda)
#   # test$sat_pois <- log_lik_pois(test$x, test$x)
#   #
#   # ggplot(test) +
#   #   geom_line(aes(x = x, y = ll), colour = "red") +
#   #   geom_line(aes(x = x, y = sat), colour = "firebrick") +
#   #   geom_line(aes(x = x, y = ll_pois), colour = "blue") +
#   #   geom_line(aes(x = x, y = sat_pois), colour = "steelblue")
#
#   # test$mean <- NA
#   # for (i in 1:nrow(test)) {
#   #   test$mean[i] <- mean_gamma_count(test$lambda[i], test$alpha[i])
#   # }
#   #
#   # test %>% dplyr::arrange(desc(ll))
# }
#
# pars2 %>% filter(!dev)
# #
# # # pars2 %>%
# # #   filter(fail_mean) %>%
# # #   ggplot() +
# # #   geom_histogram(aes(x = theta))
# #
# # pars2 %>%
# #   filter(fail_x) %>%
# #   ggplot() +
# #   geom_histogram(aes(x = theta))
# #
# # pars2 %>%
# #   filter(fail_x) %>%
# #   ggplot() +
# #   geom_histogram(aes(x = diff_x))
# #
# # # pars2 %>%
# # #   filter(fail_mean) %>%
# # #   ggplot() +
# # #   geom_histogram(aes(x = diff_mean))
# #
# # # lambda theta fail_x fail_mean    diff_x diff_mean
# # # <dbl> <dbl> <lgl>  <lgl>         <dbl> <lgl>
# # # 1   1.27  1.62 TRUE   NA        -0.00294  NA
# # # 2   1.27  2.29 TRUE   NA        -0.00175  NA
# # # 3   1.27  1.24 TRUE   NA        -0.000488 NA
# # #
# # # lambda theta fail_x fail_mean     diff_x diff_mean
# # # <dbl> <dbl> <lgl>  <lgl>          <dbl> <lgl>
# # #   1   8.26  2.71 TRUE   NA        -0.0000133 NA
# #
# # # lambda theta fail_x fail_mean       diff_x diff_mean
# # # <dbl> <dbl> <lgl>  <lgl>            <dbl> <lgl>
# # #   1   4.82 0.225 TRUE   NA        -0.000000189 NA
# #
# # # lambda theta fail_x fail_mean      diff_x diff_mean
# # # <dbl> <dbl> <lgl>  <lgl>           <dbl> <lgl>
# # # 1   7.23 3.12  TRUE   NA        -0.0000218  NA
# # # 2   1.45 0.778 TRUE   NA        -0.00106    NA
# # # 3   1.45 0.824 TRUE   NA        -0.00148    NA
# # # 4   1.45 1.20  TRUE   NA        -0.000155   NA
# # # 5   1.45 0.794 TRUE   NA        -0.00123    NA
# # # 6  13.5  1.20  TRUE   NA        -0.00000215 NA
# # # 7   5.41 1.20  TRUE   NA        -0.00000334 NA
# #
# # # lambda theta fail_x fail_mean       diff_x diff_mean
# # # <dbl> <dbl> <lgl>  <lgl>            <dbl> <lgl>
# # # 1  11.5  0.987 TRUE   NA        -0.000000263 NA
# # # 2  11.5  0.947 TRUE   NA        -0.00000475  NA
# # # 3   2.49 0.987 TRUE   NA        -0.000143    NA
# # # 4   2.49 0.947 TRUE   NA        -0.000271    NA
# #
# # # lambda theta fail_x fail_mean      diff_x diff_mean
# # # <dbl> <dbl> <lgl>  <lgl>           <dbl> <lgl>
# # # 1   9.18  3.96 TRUE   NA        -0.00000472 NA
# # # 2   9.18  4.23 TRUE   NA        -0.00000892 NA
# # # 3   3.31  1.56 TRUE   NA        -0.0000203  NA
# # # 4   4.34  1.56 TRUE   NA        -0.0000655  NA
