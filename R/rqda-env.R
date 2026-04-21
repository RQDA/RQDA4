button <- new.env(parent = emptyenv())
.rqda <- new.env(parent = emptyenv())
.codingEnv <- new.env(parent = emptyenv())

.rqda$back.col <- "gold"
.rqda$BOM <- FALSE
.rqda$TOR <- "unconditional"
.rqda$codeMark.col <- "green"
.rqda$codingTable <- "coding"
.rqda$isLaunched <- FALSE
.rqda$owner <- "default"
.rqda$encoding <- "unknown"
.rqda$fore.col <- "blue"

# Default widget size/position options used by dialogs throughout RQDA
if (is.null(getOption("widgetSize")))
  options(widgetSize = c(600, 400))
if (is.null(getOption("widgetCoordinate")))
  options(widgetCoordinate = c(100, 100))
.rqda$font <- "Sans 11"
