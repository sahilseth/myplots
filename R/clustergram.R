#' Create a cluster gram
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @details
#' 
#' \code{
#' sample_id paper_class new_class
#' ICGC_0354           1         1
#' ICGC_0033           1         1
#' }
clustergram <- function(x){
  
  cluster_names = colnames(x)[-1]
  x2 = group_by_(x, .dots = cluster_names) %>% 
    summarize(width = n(), lbl = paste(sample_id, collapse = ";")) %>% 
    ungroup() %>% arrange_(.dots = cluster_names)
  
  x2$id = 1:nrow(x2)
  x2$main_cluster = as.factor(unlist(x2[, cluster_names[1]]))
  
  x.lng = melt(x2, measure.vars = cluster_names, variable.name = "cluster_name", value.name = "cluster")
  # get number in each cluster:
  x.lng = group_by(x.lng, cluster_name, cluster) %>% mutate(tot = sum(width)) %>%
    ungroup() %>% group_by(cluster_name) %>% arrange(cluster) %>%
    mutate(y.max = cumsum(width), y.min = (y.max  - width)) %>%
    mutate(cluster_id = as.integer(cluster_name))
  
  #x2$main_cluster = unlist(x2[, cluster_names[1]])
  
  p = ggplot(x.lng) +
    geom_ribbon(aes(x = cluster_id, ymin = y.min, ymax = y.max, fill = main_cluster, group = id, label = lbl), alpha = 0.8) + 
    scale_fill_brewer(palette = "Set1", name = "")
  
  p = p + geom_bar(aes(x = as.numeric(variable), fill = as.factor(value)), data = melt(x), stat = "count", alpha = 0.4, width = 0.1) +
    xlab("") + ylab("") + 
    scale_x_continuous(labels = cluster_names, breaks = 1:length(cluster_names))
  p
}
