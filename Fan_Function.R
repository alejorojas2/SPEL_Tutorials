
plot_index <- function(index, df.sample) {
  index_sum <- df.sample %>%
    group_by(Treatment, Inoculation, Cultivar, Time) %>%
    summarise(mean_df = mean(!!sym(index)),
              sd_df = sd(!!sym(index)),
              n_df = length(!!sym(index)),
              se_df = sd(!!sym(index))/sqrt(length(!!sym(index))))
  
  plot_df <- ggplot(index_sum, aes(x = Time, y = mean_df)) +
    geom_errorbar(aes(ymin = mean_df-se_df, ymax = mean_df+se_df, color = Cultivar),
                  position = position_dodge(10)) +
    geom_point(aes(color = Cultivar, shape = Cultivar), size = 3, position = position_dodge(10)) + ylim(0.4, 0.9) +
    geom_line(aes(color = Inoculation, linetype = Cultivar)) +
    facet_wrap(Treatment ~ ., scales = "free_x") +
    scale_color_brewer(palette = "Set1", direction = ) + theme_bw(base_size = 16) +  theme(axis.text.x = element_blank())
  return(plot_df)
}

plot_index("GNDVI", anim21)

plot_index("GNDVI", anim22)

