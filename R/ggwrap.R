ggwrap <- function(plotdata, x, y, fill, xlab, ylab, main, plottype) {
  barplot <- ggplot(data = plotdata, aes(x=x, y=y, fill=fill)) +
    geom_bar(stat = "identity") +
    labs(
      x = xlab,
      y = ylab
    ) +
    scale_fill_manual(
      values = c("coral", "cyan3"),
      name = main
    ) +
    theme_minimal()

  switch (plottype,
          barplot = plot(barplot)
  )
}
