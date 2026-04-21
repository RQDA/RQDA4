# Dialog functions for RQDA - GTK4 implementation

#' Show message dialog
#' @param message Message to display
#' @param icon Icon type: "info", "warning", "error", "question"
#' @param parent Parent window (optional)
gmessage <- function(message, icon = "info", parent = NULL, container = FALSE) {

  # GTK4: Use simple message printing for now
  # Full AlertDialog implementation would require async handling
  cat(icon, ": ", message, "\n", sep = "")

  # Also use R's message system
  if (icon == "error") {
    warning(message, call. = FALSE)
  } else {
    message(message)
  }

  invisible(NULL)
}

#' Show confirmation dialog
#' @param message Question to ask
#' @param icon Icon type (usually "question")
#' @param parent Parent window (optional)
#' @return TRUE if yes, FALSE if no
gconfirm <- function(message, icon = "question", parent = NULL) {

  # For now, use R's readline for confirmation
  cat(message, " (y/n): ", sep = "")
  response <- readline()

  return(tolower(substr(response, 1, 1)) == "y")
}

#' Show input dialog
#' @param message Prompt message
#' @param text Default text
#' @param title Dialog title
#' @param parent Parent window (optional)
#' @return Character string or character(0) if cancelled
ginput <- function(message, text = "", title = "Input", parent = NULL) {

  # GTK4: Use readline for now
  # Proper implementation would need GtkWindow with entry widget
  cat(title, "\n", message, "\n", sep = "")
  if (nchar(text) > 0) {
    cat("(default: ", text, ")\n", sep = "")
  }
  cat("> ")

  response <- readline()

  # If empty and there's a default, use default
  if (nchar(response) == 0 && nchar(text) > 0) {
    return(text)
  }

  # If empty and no default, return empty
  if (nchar(response) == 0) {
    return(character(0))
  }

  return(response)
}

#' Create a simple text display window
#' @param text Text to display
#' @param title Window title
#' @param width Window width
#' @param height Window height
gtext_window <- function(text, title = "Text", width = 600, height = 400) {

  win <- gwindow(title = title, width = width, height = height)
  txt <- gtext(text, container = win)

  invisible(win)
}
