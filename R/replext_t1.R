replext_t1_c1 <- function(S = 20000, k = 3,
                          fs = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
                          n_start = 6, constrs = c(0, 1, 2), alpha = 0.05,
                          pow = 0.80) {
  # Initialize matrices to store the results for sample sizes and Type I error rates
  sample_sizes <- matrix(NA, nrow = length(constrs), ncol = length(fs),
                         dimnames = list(paste0("constr=", constrs), paste0("f=", fs)))
  type_I_errors <- rep(NA, length(constrs))

  # Iterate over effect sizes and constraint levels
  for (f in fs) {
    for (constr in constrs) {
      n <- n_start
      current_power <- 0
      while (current_power < pow && n <= S) {
        # Generate datasets
        datasets <- generate_datasets(S, k, f, n)

        # Calculate power
        current_power <- pj_pow(datasets, constr, alpha)

        # Check if the power target is met
        if (current_power >= pow) {
          sample_sizes[paste0("constr=", constr), paste0("f=", f)] <- n
          break
        }

        # Increase the sample size for the next iteration
        n <- n + 1
      }
    }
  }

  # Calculate Type I error rates
  for (constr in constrs) {
    largest_effect_size_n <- min(sample_sizes[constr, ], na.rm = TRUE)
    type_I_error_datasets <- generate_datasets(S/2, k, 0, largest_effect_size_n)
    type_I_errors[constr] <- pj_pow(type_I_error_datasets, constr, alpha)
  }

  # Combine sample sizes and Type I error rates into a data frame
  results <- as.data.frame(sample_sizes)
  results$Type_I_Error <- type_I_errors

  # Rearrange the columns to make Type_I_Error the first column
  results <- results[, c("Type_I_Error", setdiff(names(results), "Type_I_Error"))]

  return(results)
}


# replext_t1_c1 <- function(S = 20000, k = 3,
#                           fs = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
#                           n_start = 6, constrs = c(0, 1, 2), alpha = 0.05,
#                           pow = 0.80) {
#   # Initialize a matrix to store the results for sample sizes
#   sample_sizes <- matrix(NA, nrow = length(constrs), ncol = length(fs),
#                          dimnames = list(paste0("constr=", constrs), paste0("f=", fs)))
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
#   # Calculate Type I error rate using the sample size for the largest f and S/2 datasets
#   largest_effect_size_n <- min(sample_sizes, na.rm = TRUE)
#   type_I_error_datasets <- generate_datasets(S/2, k, max(fs), largest_effect_size_n)
#   type_I_error_rate <- pj_pow(type_I_error_datasets, max(constrs), alpha)
#
#   # Combine sample sizes and Type I error rate into a data frame
#   results <- as.data.frame(sample_sizes)
#   results$Type_I_Error <- type_I_error_rate
#
#   return(results)
# }


