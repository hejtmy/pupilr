rename_column <- function(df, old_column, new_column){
  colnames(df)[colnames(df) %in% c(old_column)] <- new_column
  return(df)
}
