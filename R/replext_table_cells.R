replext_t1_c1 <- function(S = 20000, k = 3,
                          fs = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
                          n_start = 6, constrs = c(0, 1, 2), alpha = 0.05,
                          pow = 0.80, nmax = 1000) {
  # Vectorized function to find the minimum sample size for a given power and effect size
  find_min_sample_size <- function(f, constr) {
    n <- n_start
    current_power <- 0.0
    while (current_power < pow && n <= nmax) {
      datasets <- generate_datasets(S, k, f, n)
      current_power <- pj_pow(datasets, constr, alpha)
      if (current_power >= pow) {
        return(n)
      }
      n <- n + 1
    }
    return(NA)
  }


  # Create a matrix for sample sizes using outer
  sample_sizes <- outer(fs, constrs, Vectorize(find_min_sample_size))
  dimnames(sample_sizes) <- list(paste0("f=", fs), paste0("constr=", constrs))

  # Function to calculate Type I error rates
  calc_type_I_error <- function(constr) {
    largest_effect_size_n <- min(sample_sizes[, paste0("constr=", constr)], na.rm = TRUE)
    S_half <- as.integer(S/2)
    type_I_error_datasets <- generate_datasets(S_half, k, 0, largest_effect_size_n)
    pj_pow(type_I_error_datasets, constr, alpha)
  }

  # Calculate Type I error rates
  type_I_errors <- sapply(constrs, calc_type_I_error)

  # Combine and rearrange the results
  results <- cbind(Type_I_Error = type_I_errors, as.data.frame(t(sample_sizes)))

  return(results)
}


replext_t2_c1 <- function(S = 20000, p = 3,
                          f2s = c(0.02, 0.05, 0.08, 0.10, 0.15, 0.20, 0.25, 0.35),
                          n_start = 6, constrs = c(0, 1, 2, 3), rho = 0.0, beta = 0.1,
                          alpha = 0.05, pow = 0.80, standardize = TRUE,nmax = 1000) {
  # Vectorized function to find the minimum sample size for a given power and effect size
  find_min_sample_size <- function(f2, constr) {
    n <- n_start
    current_power <- 0
    while (current_power < pow && n <= nmax) {
      datasets <- generate_datasets_reg(S, n, p, f2, rho, beta)
      current_power <- lr_pow(datasets, constr, standardize, alpha)
      if (current_power >= pow) {
        return(n)
      }
      n <- n + 1
    }
    return(NA)
  }

  # Create a matrix for sample sizes using outer
  sample_sizes <- outer(f2s, constrs, Vectorize(find_min_sample_size))
  dimnames(sample_sizes) <- list(paste0("f2=", f2s), paste0("constr=", constrs))

  # Function to calculate Type I error rates
  calc_type_I_error <- function(constr) {
    largest_effect_size_n <- min(sample_sizes[, paste0("constr=", constr)], na.rm = TRUE)
    S_half <- as.integer(S/2)
    type_I_error_datasets <- generate_datasets_reg(S_half, largest_effect_size_n, p, .Machine$double.eps, rho, beta)
    lr_pow(type_I_error_datasets, constr, standardize, alpha)
  }

  # Calculate Type I error rates
  type_I_errors <- sapply(constrs, calc_type_I_error)

  # Combine and rearrange the results
  results <- cbind(Type_I_Error = type_I_errors, as.data.frame(t(sample_sizes)))

  return(results)
}

