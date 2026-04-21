# File-related buttons for RQDA

ImportFileButton <- function(label, container, ...) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        ImportFileText()
      }, error = function(e) {
        message("Error importing file: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("ImpFilB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

NewFileButton <- function(label, container, ...) {
  if (!exists("button", envir = .GlobalEnv)) {
    assign("button", new.env(), envir = .GlobalEnv)
  }

  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      # Just call NewFile. It handles the popup AND the Editor launch.
      NewFile()
    }
  )

  assign("NewFilB", btn, envir = get("button", envir = .GlobalEnv))
  btn
}

DeleteFileButton <- function(label, container, ...) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        DeleteFile()
      }, error = function(e) {
        message("Error deleting file: ", e$message)
      })
    }
  )

  btn
}

ViewFileButton <- function(label, container, ...) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        ViewFile()
      }, error = function(e) {
        message("Error viewing file: ", e$message)
      })
    }
  )

  btn
}

File_MemoButton <- function(label, container, FileWidget, ...) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        FileMemo()
      }, error = function(e) {
        message("Error opening file memo: ", e$message)
      })
    }
  )

  btn
}

File_RenameButton <- function(label, container, FileWidget, ...) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        RenameFile()
      }, error = function(e) {
        message("Error renaming file: ", e$message)
      })
    }
  )

  btn
}
