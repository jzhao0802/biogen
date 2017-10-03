config_with_colnames <- function(config, colnameslist) {
  
  Type = hashmap(config$Column, config$Type)
  NumUniqueValues = hashmap(config$Column, config$NumUniqueValues)
  Description = hashmap(config$Column, config$Description)
  isdate = hashmap(config$Column, config$isdate)
  var_grouping = hashmap(config$Column, config$var_grouping)
  var_period = hashmap(config$Column, config$var_period)
  
  config_ = tibble(Column = colnameslist, 
                   Type = Type[[colnameslist]],
                   NumUniqueValues = NumUniqueValues[[colnameslist]],
                   Description = Description[[colnameslist]],
                   isdate = isdate[[colnameslist]],
                   var_grouping = var_grouping[[colnameslist]], 
                   var_period = var_period[[colnameslist]])
  
  return(config_)
}