config_with_colnames_with_v <- function(config, colnameslist) {
  
  Type = hashmap(config$Column, config$Type)
  NumUniqueValues = hashmap(config$Column, config$NumUniqueValues)
  Description = hashmap(config$Column, config$Description)
  isdate = hashmap(config$Column, config$isdate)
  var_grouping = hashmap(config$Column, config$var_grouping)
  var_period = hashmap(config$Column, config$var_period)
  v0 = hashmap(config$Column, config$v0)
  v1 = hashmap(config$Column, config$v1)
  v2 = hashmap(config$Column, config$v2)
  v3 = hashmap(config$Column, config$v3)
  v4 = hashmap(config$Column, config$v4)
  
  config_ = tibble(Column = colnameslist, 
                   Type = Type[[colnameslist]],
                   NumUniqueValues = NumUniqueValues[[colnameslist]],
                   Description = Description[[colnameslist]],
                   isdate = isdate[[colnameslist]],
                   var_grouping = var_grouping[[colnameslist]], 
                   var_period = var_period[[colnameslist]],
                   v0 = v0[[colnameslist]],
                   v1 = v1[[colnameslist]],
                   v2 = v2[[colnameslist]],
                   v3 = v3[[colnameslist]],
                   v4 = v4[[colnameslist]])
  
  return(config_)
}