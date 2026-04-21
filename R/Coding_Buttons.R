# Coding-related buttons for RQDA

AddCodeButton <- function(label = gettext("Add", domain = "R-RQDA")) {
  btn <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        AddCode()
      }, error = function(e) {
        message("Error adding code: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("AddCodB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

DeleteCodeButton <- function(label = gettext("Delete", domain = "R-RQDA")) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        DeleteCode()
      }, error = function(e) {
        message("Error deleting code: ", e$message)
      })
    }
  )

  button
}

FreeCode_RenameButton <- function(label, CodeNamesWidget, ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        RenameCode()
      }, error = function(e) {
        message("Error renaming code: ", e$message)
      })
    }
  )

  button
}

CodeMemoButton <- function(label = gettext("C-Memo", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        CodeMemo()
      }, error = function(e) {
        message("Error opening code memo: ", e$message)
      })
    }
  )

  button
}

AnnotationButton <- function(label = gettext("Add Anno", domain = "R-RQDA")) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        AddAnnotation()
      }, error = function(e) {
        message("Error adding annotation: ", e$message)
      })
    }
  )

  button
}

RetrievalButton <- function(label) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        RetrieveCoding()
      }, error = function(e) {
        message("Error retrieving codings: ", e$message)
      })
    }
  )

  button
}

Unmark_Button <- function(name = NULL, label = gettext("UnMark", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        UnmarkCoding()
      }, error = function(e) {
        message("Error unmarking: ", e$message)
      })
    }
  )

  button
}

Mark_Button <- function(name = NULL, label = gettext("Mark", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        MarkCoding()
      }, error = function(e) {
        message("Error marking: ", e$message)
      })
    }
  )

  button
}
