risk_plot <-function (risk_df, prior, class_labels = c("TRUE", "FALSE")) {

  risk_df %>% ggplot2::ggplot(ggplot2::aes(levels, class_rate, 
                                           color = below_prior)) + ggplot2::geom_point(size = 7) + 
    ggplot2::geom_hline(yintercept = prior, color = "darkgray") + 
    ggplot2::geom_segment(ggplot2::aes(x = factor(levels, 
                                                  unique(levels)), xend = levels, y = prior, yend = class_rate), 
                          size = 0.5) + ggplot2::geom_col(ggplot2::aes(levels, 
                                                                       level_prop), alpha = 0.3, color = "gray") + ggplot2::theme_minimal() + 
    ggplot2::scale_color_manual(values = c(`TRUE` = "firebrick2", 
                                           `FALSE` = "darkolivegreen2")) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                                                              hjust = 1), legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5, 
                                                                                                                                                                                                       size = 10), panel.grid.major.x = ggplot2::element_blank()) + 
    ggplot2::ggtitle(paste("Categories size (bars) and \n", 
                           class_labels[1], "class rate (balloons)")) + ggplot2::ylab("") + 
    ggplot2::xlab("")
}

category_risk_df <- function (target, feature, prior) 
{
  if (is.numeric(feature)) {
    feature <- discretize_num(feature, equal_count_bins = T)
  }
  tibble::tibble(levels = feature, target = target) %>% dplyr::group_by(levels) %>% 
    dplyr::summarise(level_size = n(), level_prop = level_size/nrow(.), 
                     class_rate = sum(target)/n(), below_prior = class_rate < 
                       prior, distance_from_prior = abs(class_rate - 
                                                          prior))
}


discretize_num <- function(var, breaks = 10, equal_count_bins = F){
  if(equal_count_bins){
    res <- cut(var,
               unique(quantile(var, seq(0,1,(1/breaks)), na.rm = T)),
               include.lowest = T,
               dig.lab = 3)
  }else{
    res <- cut(var, breaks = breaks, dig.lab = 3)
  }
  res
}


validate_feature_input <- function (target, feature)  
{
  if (anyNA(target)) 
    stop("target can't contain NA values")
  if (length(feature) != length(target)) 
    stop("target and feature are not the same length")
}


calc_info_gain <- function (target, feature, bins = 10) 
{
  validate_feature_input(target, feature)
  complete_cases <- !is.na(feature)
  target <- target[complete_cases]
  feature <- feature[complete_cases]
  if (is.numeric(target)) 
    warning("Target variable is numeric, each unique number is considered as a factor.")
  if (is.numeric(feature)) 
    feature <- cut_f(feature, bins)
  if (is.factor(feature)) 
    feature <- droplevels(feature)
  info_gain_disc(target, feature)
}

info_gain_disc <- function (target, feature) 
{
  H_target <- entropy::entropy(y = table(target), unit = "log2")
  p_feature <- prop.table(table(feature))
  table2d <- table(feature, target)
  H_feature <- apply(table2d, 1, function(x) entropy::entropy(x, 
                                                              unit = "log2"))
  H_target - sum(p_feature * H_feature)
}

cut_f <- function (feature, bins) 
{
  cut(feature, breaks = bins, dig.lab = 10)
}



plot_density_to_y <- function(feature) {
  ggplot(train_prepared, aes(x = feature, fill = y)) +
    geom_density(alpha = 0.9)
}

