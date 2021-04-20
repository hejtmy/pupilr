rename_column <- function(df, old_column, new_column){
  colnames(df)[colnames(df) %in% c(old_column)] <- new_column
  return(df)
}

downsample <- function(df, n){
  if(n < 2 ) return(df)
  if(nrow(df) < 1) return(df)
  df <- df[seq(1, nrow(df), n), ]
  return(df)
}
