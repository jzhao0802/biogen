factor2numeric <- function(attribute) {
  attribute= as.factor(attribute)
  attribute_levels = levels(attribute)
  attribute_num = factor(
    attribute,
    levels = attribute_levels,
    labels = sprintf("%03d", 1:length(attribute_levels))
    )
  
  attribute_map = hashmap(levels(attribute_num), attribute_levels)
  return(list(map=attribute_map, value=attribute_num))
}