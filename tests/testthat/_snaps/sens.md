# sens_norm returns a numeric vector of length 2 with correct names

    Code
      sens_norm(mean = 2, sd = 10, sd_mult = 2)
    Output
      $mean
      [1] 2
      
      $sd
      [1] 20
      

# sens_student returns a numeric vector of length 3 with correct names

    Code
      sens_student(mean = 2, sd = 10, theta = 0.1, sd_mult = 2)
    Output
      $mean
      [1] 2
      
      $sd
      [1] 20
      
      $theta
      [1] 0.1
      

# sens_exp returns a numeric vector of length 1 with correct names

    Code
      sens_exp(rate = 2, sd_mult = 2)
    Output
      $rate
      [1] 1
      

# sens_skewnorm returns a numeric vector of length 3 with correct names

    Code
      sens_skewnorm(mean = 2, sd = 10, shape = -1, sd_mult = 2)
    Output
      $mean
      [1] 7.641896
      
      $sd
      [1] 20
      
      $shape
      [1] -1
      

# sens_lnorm returns a numeric vector of length 2 with correct names

    Code
      sens_lnorm(meanlog = 2, sdlog = 10, sd_mult = 2)
    Output
      $meanlog
      [1] 1.306853
      
      $sdlog
      [1] 10.06908
      

# sens_beta returns a numeric vector of length 2 with correct names

    Code
      sens_beta(alpha = 2, beta = 10, sd_mult = 2)
    Output
      $alpha
      [1] 1
      
      $beta
      [1] 1
      

# sens_pois returns a numeric vector of length 1 with correct names

    Code
      sens_pois(lambda = 10, sd_mult = 2)
    Output
      $lambda
      [1] 40
      

# sens_gamma returns a numeric vector of length 2 with correct names

    Code
      sens_gamma(shape = 2, rate = 10, sd_mult = 2)
    Output
      $shape
      [1] 0.5
      
      $rate
      [1] 2.5
      

# sens_gamma_pois returns a numeric vector of length 2 with correct names

    Code
      sens_gamma_pois(lambda = 10, theta = 0.1, sd_mult = 2)
    Output
      $lambda
      [1] 10
      
      $theta
      [1] 0.7
      

# sens_neg_binom returns a numeric vector of length 2 with correct names

    Code
      sens_neg_binom(lambda = 10, theta = 0.2, sd_mult = 2)
    Output
      $lambda
      [1] 10
      
      $theta
      [1] 1.1
      

# sens_gamma_pois_zi returns a numeric vector of length 3 with correct names

    Code
      sens_gamma_pois_zi(lambda = 2, theta = 0.2, prob = 0.3, sd_mult = 2)
    Output
      $lambda
      [1] 2
      
      $theta
      [1] 3.2
      
      $prob
      [1] 0.3
      

