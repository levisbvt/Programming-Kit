\name{ggwrap}
\alias{ggwrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ggwrap
}
\usage{
ggwrap(plotdata, x, y, fill, xlab, ylab, main, plottype)
}
\description{
Wrapper for ggplot, includes a few pre-configured plots.
}
\examples{
ggwrap(data, data$Weight, data$Height, data$Sex, "weight", "height", "Weight and height per sex", barplot)
}
