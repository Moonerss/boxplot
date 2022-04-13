#' @title convert input file to plot format
#' @param mat input matrix
#' @param group group file

data_convert <- function(mat, group) {
  dat <- mat %>% as.data.frame() %>% 
    rownames_to_column(var = 'Genes') %>% 
    pivot_longer(cols = -Genes) %>% 
    left_join(group, by = c('name' = 'ID'))
  return(dat)
}