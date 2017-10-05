make_var_name <- function(colnamelist) {
  
  colnamelist = str_replace_all(colnamelist,' ','_')
  colnamelist = str_replace_all(colnamelist,'-','_')
  colnamelist = str_replace_all(colnamelist,'&','n')
  colnamelist = str_replace_all(colnamelist,',','__')
  colnamelist = str_replace(colnamelist,'\\(','_')
  colnamelist = str_replace_all(colnamelist,'\\)','_')
  colnamelist = str_replace_all(colnamelist,'\\/','_or_')
  colnamelist = str_replace_all(colnamelist,'\\r','')
  
  return( colnamelist )
}