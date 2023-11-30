replext_t1_c1 <- function(S = 20000, k = 3,
                          fs = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
                          n_start = 6, constrs = c(0, 1, 2), alpha = 0.05,
                          pow = 0.80) {
  # Vectorized function to find the minimum sample size for a given power and effect size
  find_min_sample_size <- function(f, constr) {
    n <- n_start
    current_power <- 0
    while (current_power < pow && n <= S) {
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
    largest_effect_size_n <- min(sample_sizes[, constr], na.rm = TRUE)
    type_I_error_datasets <- generate_datasets(S/2, k, 0, largest_effect_size_n)
    pj_pow(type_I_error_datasets, constr, alpha)
  }

  # Calculate Type I error rates
  type_I_errors <- sapply(constrs, calc_type_I_error)

  # Combine and rearrange the results
  results <- cbind(Type_I_Error = type_I_errors, as.data.frame(t(sample_sizes)))

  return(results)
}


# replext_t1_c1 <- function(S = 20000, k = 3,
#                           fs = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
#                           n_start = 6, constrs = c(0, 1, 2), alpha = 0.05,
#                           pow = 0.80) {
#   # Initialize matrices to store the results for sample sizes and Type I error rates
#   sample_sizes <- matrix(NA, nrow = length(constrs), ncol = length(fs),
#                          dimnames = list(paste0("constr=", constrs), paste0("f=", fs)))
#   type_I_errors <- rep(NA, length(constrs))
#
#   # Iterate over effect sizes and constraint levels
#   for (f in fs) {
#     for (constr in constrs) {
#       n <- n_start
#       current_power <- 0
#       while (current_power < pow && n <= S) {
#         # Generate datasets
#         datasets <- generate_datasets(S, k, f, n)
#
#         # Calculate power
#         current_power <- pj_pow(datasets, constr, alpha)
#
#         # Check if the power target is met
#         if (current_power >= pow) {
#           sample_sizes[paste0("constr=", constr), paste0("f=", f)] <- n
#           break
#         }
#
#         # Increase the sample size for the next iteration
#         n <- n + 1
#       }
#     }
#   }
#
#   # Calculate Type I error rates
#   for (constr in constrs) {
#     largest_effect_size_n <- min(sample_sizes[constr, ], na.rm = TRUE)
#     type_I_error_datasets <- generate_datasets(S/2, k, 0, largest_effect_size_n)
#     type_I_errors[constr] <- pj_pow(type_I_error_datasets, constr, alpha)
#   }
#
#   # Combine sample sizes and Type I error rates into a data frame
#   results <- as.data.frame(sample_sizes)
#   results$Type_I_Error <- type_I_errors
#
#   # Rearrange the columns to make Type_I_Error the first column
#   results <- results[, c("Type_I_Error", setdiff(names(results), "Type_I_Error"))]
#
#   return(results)
# }


