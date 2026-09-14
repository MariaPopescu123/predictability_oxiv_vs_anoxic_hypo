#' @param continuous_var name of the continuous variable to convert from
#' @param binary_var name of the binary variable to convert to
#' @param forecast  a standard VERA forecast file (All columns)
#' @param site single site ID
#' @param depth single depth
#' @param targets used to check if the var is actually a target variable
#' @param threshold what is the threshold to check against
#' @return returns a df in the VERA standard format
convert_continuous_binary <- function(continuous_var,
                                      binary_var,
                                      forecast,
                                      site,
                                      depth,
                                      targets,
                                      threshold) {
  
  
  if (forecast$family[1] == 'normal') {
    subset_forecast <- forecast |>
      filter(variable == continuous_var,
             site_id == site,
             depth_m == depth)
    
    if (nrow(subset_forecast) == 0) {
      message("A forecast for ", continuous_var,  " at ", site, " doesn't exist in this forecast table. \nCheck site and variable names.")
      return(NULL)
    } else {
      binary_forecast <- subset_forecast |>
        pivot_wider(names_from = parameter,
                    values_from = prediction) |>
        mutate(prob = pnorm(q = threshold, mean = mu, sd = sigma, lower.tail = F),
               family = 'bernoulli',
               variable = binary_var) |>
        select(!any_of(c('mu', 'sigma'))) |>
        pivot_longer(prob,
                     names_to = 'parameter',
                     values_to = 'prediction')
      if (nrow(targets |> distinct(variable) |> filter(variable == binary_var)) == 1) {
        message('Converted ', continuous_var, ' to ', binary_var, ' at ', site)
        return(binary_forecast)
      } else {
        message(binary_var,' is not a target variable.')
      }
      
    }
    
    
  }
  
  if (forecast$family[1] == 'ensemble') {
    subset_forecast <- forecast |>
      filter(variable == continuous_var,
             site_id == site,
             depth_m == depth)
    
    if (nrow(subset_forecast) == 0) {
      message("A forecast for ", continuous_var,  " at ", site, " doesn't exist in this forecast table. \nCheck site and variable names.")
      return(NULL)
    } else {
      binary_forecast <-
        subset_forecast |>
        mutate(above_threshold = prediction > threshold) |>
        group_by(model_id, datetime, reference_datetime, site_id, variable, project_id, duration, depth_m) |>
        summarise(n = sum(above_threshold), total = n(), .groups = 'drop') |>
        mutate(prediction = n/total ,
               parameter = 'prob',
               family = 'bernoulli') |>
        select(-any_of(c('n', 'total')))
      
      
      if (nrow(targets |> distinct(variable) |> filter(variable == binary_var)) == 1) {
        message('Converted ', continuous_var, ' to ', binary_var, ' at ', site)
        return(binary_forecast)
      } else {
        message(binary_var,' is not a target variable.')
      }
      
    }
    
    
  }
  
  
}