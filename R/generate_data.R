#' Generate Normal-Normal Data
#'
#' Function to generate data according to normal-normal heirarchical model
#' @param num_groups Number of groups or clusters
#' @keywords simulate data
#' @export
#' @examples
#' generate_normal_normal_data()

generate_normal_normal_data <- function(num_groups,
                                        sample_sizes=NULL,
                                        min_sample_size=1,
                                        max_sample_size=100,
                                        hyper_mean=0,
                                        hyper_var=1, data_var=1, seed=NULL) {
    if(!is.null(seed))
        set.seed(seed)
    ## TODO: check if sample_sizes are valid
    if(is.null(sample_sizes))
        sample_sizes <- sample(min_sample_size:max_sample_size,
                               size=num_groups, replace=TRUE)  # sample size per group
    ## simulate group means
    group_means <- rnorm(num_groups, hyper_mean, sqrt(hyper_var))
    ## simulate observed values
    data <- mapply(function(n, m) rnorm(n, m, sqrt(data_var)), sample_sizes, group_means)
    ## calculate sample means (sufficient statistics)
    group_sample_means <- sapply(data, mean)
    ## return result
    list(hyper_mean=hyper_mean,
         hyper_var=hyper_var,
         data_var=data_var,
         sample_sizes=sample_sizes,
         group_means=group_means,
         group_sample_means=group_sample_means,
         data=data
         )
}
