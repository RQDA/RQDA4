# Project-related buttons for RQDA

NewProjectButton <- function(container) {
  btn <- gbutton(
    rqda_txt("New Project"),
    container = container,
    handler = function(h, ...) {
      path <- gfile(
        type = "save",
        text = rqda_txt("Type a name for the new project and click OK.")
      )

      if (!is.null(path) && !identical(path, character(0)) && nzchar(path)) {
        if (Encoding(path) != "UTF-8") {
          Encoding(path) <- "UTF-8"
        }

        # Create new project
        tryCatch({
          new_proj(path, assignenv = .rqda)

          # Update UI
          path <- .rqda$qdacon@dbname
          Encoding(path) <- "UTF-8"
          path <- gsub("\\\\", "/", path, fixed = TRUE)
          path <- gsub("/", "/ ", path, fixed = TRUE)

          .proj_path_val <- gsub(
            "/ ", "/",
            paste(strwrap(path, 60), collapse = "\n"),
            fixed = TRUE
          )
          gtkLabelSetMarkup(.rqda$.currentProj$widget,
                            sprintf("<span foreground='#888888' size='small'>%s</span>",
                                    gsub("&","&amp;", .proj_path_val)))

          # Enable various UI elements
          if (exists("button", envir = .GlobalEnv)) {
            enabled(button$cloprob) <- TRUE
            enabled(button$BacProjB) <- TRUE
            enabled(button$saveAsB) <- TRUE
            enabled(button$proj_memo) <- TRUE
            enabled(button$CleProB) <- TRUE
            enabled(button$CloAllCodB) <- TRUE
            enabled(button$ImpFilB) <- TRUE
            enabled(button$NewFilB) <- TRUE
            enabled(button$AddJouB) <- TRUE
            enabled(button$AddCodB) <- TRUE
            enabled(button$AddCodCatB) <- TRUE
            enabled(button$AddCasB) <- TRUE
            enabled(button$AddAttB) <- TRUE
            enabled(button$AddFilCatB) <- TRUE
          }

          if (exists(".rqda", envir = .GlobalEnv)) {
            enabled(.rqda$.fnames_rqda) <- TRUE
            enabled(.rqda$.JournalNamesWidget) <- TRUE
            enabled(.rqda$.codes_rqda) <- TRUE
            enabled(.rqda$.SettingsGui) <- TRUE
            enabled(.rqda$.CodeCatWidget) <- TRUE
            enabled(.rqda$.CasesNamesWidget) <- TRUE
            enabled(.rqda$.AttrNamesWidget) <- TRUE
            enabled(.rqda$.FileCatWidget) <- TRUE
          }
        }, error = function(e) {
          message("Error creating project: ", e$message)
        })
      }
    }
  )

  # Store button reference
  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }

  btn
}

OpenProjectButton <- function(container) {
  btn <- gbutton(
    rqda_txt("Open Project"),
    container = container,
    handler = function(h, ...) {
      path <- gfile(
        text = rqda_txt("Select a *.rqda file and click OK."),
        type = "open",
        filter = list(
          "rqda" = list(patterns = c("*.rqda")),
          "All files" = list(patterns = c("*"))
        )
      )

      if (!identical(path, character(0)) && !is.na(path)) {
        Encoding(path) <- "UTF-8"
        tryCatch({
          openProject(path, updateGUI = TRUE)
        }, error = function(e) {
          message("Error opening project: ", e$message)
        })
      }
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }

  btn
}

#' @export
openProject <- function(path, updateGUI = FALSE) {
  # Clear existing data - only if widgets exist
  if (exists(".codes_rqda", envir = .rqda)) {
    tryCatch(.rqda$.codes_rqda[] <- NULL, error = function(e) {
      message("Could not clear codes widget: ", e$message)
    })
  }
  if (exists(".fnames_rqda", envir = .rqda)) {
    tryCatch(.rqda$.fnames_rqda[] <- NULL, error = function(e) {
      message("Could not clear files widget: ", e$message)
    })
  }

  # Now these widgets exist - clear them too
  if (exists(".CasesNamesWidget", envir = .rqda)) {
    tryCatch(.rqda$.CasesNamesWidget[] <- NULL, error = function(e) {})
  }
  if (exists(".CodeCatWidget", envir = .rqda)) {
    tryCatch(.rqda$.CodeCatWidget[] <- NULL, error = function(e) {})
  }
  if (exists(".CodeofCat", envir = .rqda)) {
    tryCatch(.rqda$.CodeofCat[] <- NULL, error = function(e) {})
  }
  if (exists(".FileCatWidget", envir = .rqda)) {
    tryCatch(.rqda$.FileCatWidget[] <- NULL, error = function(e) {})
  }
  if (exists(".FileofCat", envir = .rqda)) {
    tryCatch(.rqda$.FileofCat[] <- NULL, error = function(e) {})
  }
  if (exists(".AttrNamesWidget", envir = .rqda)) {
    tryCatch(.rqda$.AttrNamesWidget[] <- NULL, error = function(e) {})
  }
  if (exists(".JournalNamesWidget", envir = .rqda)) {
    tryCatch(.rqda$.JournalNamesWidget[] <- NULL, error = function(e) {})
  }

  # Close current project
  tryCatch(closeProject(assignenv = .rqda), error = function(e) {
    message("Error closing previous project: ", e$message)
  })

  # Open new project
  open_proj(path, assignenv = .rqda)

  if (updateGUI) {
    gtkLabelSetMarkup(.rqda$.currentProj$widget, sprintf("<span foreground='#888888' size='small'>%s</span>", rqda_txt("Opening ...")))

    # Update tables
    tryCatch(UpgradeTables(), error = function(e) {
      message("Error upgrading tables: ", e$message)
    })
    tryCatch(CodeNamesUpdate(sortByTime = FALSE), error = function(e) {
      message("Error updating code names: ", e$message)
    })
    tryCatch(FileNamesUpdate(sortByTime = FALSE), error = function(e) {
      message("Error updating file names: ", e$message)
    })

    # Now these widgets exist - can call their updates
    tryCatch(CaseNamesUpdate(), error = function(e) {
      message("Error updating case names: ", e$message)
    })
    tryCatch(UpdateTableWidget(Widget = ".CodeCatWidget", FromdbTable = "codecat"), error = function(e) {
      message("Error updating code categories: ", e$message)
    })
    tryCatch(UpdateCodeofCatWidget(), error = function(e) {
      message("Error updating codes of category: ", e$message)
    })
    tryCatch(UpdateTableWidget(Widget = ".FileCatWidget", FromdbTable = "filecat"), error = function(e) {
      message("Error updating file categories: ", e$message)
    })
    tryCatch(UpdateFileofCatWidget(), error = function(e) {
      message("Error updating files of category: ", e$message)
    })
    tryCatch(AttrNamesUpdate(), error = function(e) {
      message("Error updating attributes: ", e$message)
    })
    tryCatch(JournalNamesUpdate(), error = function(e) {
      message("Error updating journals: ", e$message)
    })

    # Update project path display
    path <- .rqda$qdacon@dbname
    Encoding(path) <- "UTF-8"
    path <- gsub("\\\\", "/", path, fixed = TRUE)
    path <- gsub("/", "/ ", path, fixed = TRUE)

    .proj_path_val <- gsub(
      "/ ", "/",
      paste(strwrap(path, 60), collapse = "\n"),
      fixed = TRUE
    )
    gtkLabelSetMarkup(.rqda$.currentProj$widget,
                      sprintf("<span foreground='#888888' size='small'>%s</span>",
                              gsub("&","&amp;", .proj_path_val)))

    # Enable UI elements - only if they exist
    if (exists("button", envir = .GlobalEnv)) {
      button_env <- get("button", envir = .GlobalEnv)
      # Only enable widgets that actually exist
      if (exists("cloprob", envir = button_env)) enabled(button_env$cloprob) <- TRUE
      if (exists("BacProjB", envir = button_env)) enabled(button_env$BacProjB) <- TRUE
      if (exists("saveAsB", envir = button_env)) enabled(button_env$saveAsB) <- TRUE
      if (exists("proj_memo", envir = button_env)) enabled(button_env$proj_memo) <- TRUE
      if (exists("CleProB", envir = button_env)) enabled(button_env$CleProB) <- TRUE
      if (exists("CloAllCodB", envir = button_env)) enabled(button_env$CloAllCodB) <- TRUE
      if (exists("ImpFilB", envir = button_env)) enabled(button_env$ImpFilB) <- TRUE
      if (exists("NewFilB", envir = button_env)) enabled(button_env$NewFilB) <- TRUE
      if (exists("AddJouB", envir = button_env)) enabled(button_env$AddJouB) <- TRUE
      if (exists("AddCodB", envir = button_env)) enabled(button_env$AddCodB) <- TRUE
      if (exists("AddCodCatB", envir = button_env)) enabled(button_env$AddCodCatB) <- TRUE
      if (exists("AddCasB", envir = button_env)) enabled(button_env$AddCasB) <- TRUE
      if (exists("AddAttB", envir = button_env)) enabled(button_env$AddAttB) <- TRUE
      if (exists("AddFilCatB", envir = button_env)) enabled(button_env$AddFilCatB) <- TRUE
    }

    if (exists(".fnames_rqda",        envir = .rqda)) enabled(.rqda$.fnames_rqda)        <- TRUE
    if (exists(".codes_rqda",         envir = .rqda)) enabled(.rqda$.codes_rqda)         <- TRUE
    if (exists(".JournalNamesWidget", envir = .rqda)) enabled(.rqda$.JournalNamesWidget) <- TRUE
    if (exists(".CodeCatWidget",      envir = .rqda)) enabled(.rqda$.CodeCatWidget)      <- TRUE
    if (exists(".CasesNamesWidget",   envir = .rqda)) enabled(.rqda$.CasesNamesWidget)   <- TRUE
    if (exists(".AttrNamesWidget",    envir = .rqda)) enabled(.rqda$.AttrNamesWidget)    <- TRUE
    if (exists(".FileCatWidget",      envir = .rqda)) enabled(.rqda$.FileCatWidget)      <- TRUE

    # Update window title
    tryCatch(gtkWindowSetTitle(.rqda$.rqda_window, sprintf("RQDA - %s", tryCatch(basename(.rqda$qdacon@dbname), error=function(e) "Untitled"))), error=function(e) {})

    # Switch to Files tab
    if (exists(".nb_rqdagui", envir = .GlobalEnv))
      tryCatch(gtkNotebookSetCurrentPage(.nb_rqdagui$widget, 1L), error = function(e) {})
  }
}

CloseProjectButton <- function(container) {
  btn <- gbutton(
    rqda_txt("Close Project"),
    container = container,
    handler = function(h, ...) {
      tryCatch({
        closeProject(assignenv = .rqda)
        gtkLabelSetMarkup(.rqda$.currentProj$widget, sprintf("<span foreground='#888888' size='small'>%s</span>", rqda_txt("No project is open.")))
        tryCatch(gtkWindowSetTitle(.rqda$.rqda_window, "RQDA"), error=function(e) {})

        # Disable UI elements
        if (exists("button", envir = .GlobalEnv)) {
          button_env <- get("button", envir = .GlobalEnv)
          enabled(button_env$cloprob) <- FALSE
          enabled(button_env$BacProjB) <- FALSE
          enabled(button_env$saveAsB) <- FALSE
          enabled(button_env$proj_memo) <- FALSE
          enabled(button_env$CleProB) <- FALSE
          enabled(button_env$CloAllCodB) <- FALSE
        }
      }, error = function(e) {
        message("Error closing project: ", e$message)
      })
    }
  )

  # Get or create button environment
  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("cloprob", btn, envir = button_env)

  # Initially disabled
  enabled(btn) <- FALSE

  btn
}

Proj_MemoButton <- function(label, container) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        ProjectMemo()
      }, error = function(e) {
        message("Error opening project memo: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("proj_memo", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

BackupProjectButton <- function(container) {
  btn <- gbutton(
    rqda_txt("Backup Project"),
    container = container,
    handler = function(h, ...) {
      tryCatch({
        BackupProject()
      }, error = function(e) {
        message("Error backing up project: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("BacProjB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

saveAsButt <- function(label, container) {
  btn <- gbutton(
    label,
    container = container,
    handler = function(h, ...) {
      tryCatch({
        saveAsProject()
      }, error = function(e) {
        message("Error in save as: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("saveAsB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

CleanProjButton <- function(container) {
  btn <- gbutton(
    rqda_txt("Clean Project"),
    container = container,
    handler = function(h, ...) {
      tryCatch({
        # Confirmation dialog would go here
        if (gconfirm("Remove all deleted items permanently?", icon="question")) {
          rqda_exe("DELETE FROM coding WHERE status=0")
          rqda_exe("DELETE FROM freecode WHERE status=0")
          rqda_exe("DELETE FROM source WHERE status=0")
          rqda_exe("DELETE FROM cases WHERE status=0")
          rqda_exe("DELETE FROM journal WHERE status=0")
          rqda_exe("DELETE FROM codecat WHERE status=0")
          rqda_exe("DELETE FROM filecat WHERE status=0")
          rqda_exe("DELETE FROM treecode WHERE status=0")
          rqda_exe("DELETE FROM treefile WHERE status=0")
          rqda_exe("DELETE FROM caselinkage WHERE status=0")
          rqda_exe("DELETE FROM annotation WHERE status=0")
          rqda_exe("VACUUM")
          message("Project cleaned.")
        }
      }, error = function(e) {
        message("Error: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("CleProB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

CloseAllCodingsButton <- function(container) {
  btn <- gbutton(
    rqda_txt("Close All Codings"),
    container = container,
    handler = function(h, ...) {
      tryCatch({
        CloseAllCoding()
      }, error = function(e) {
        message("Error: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("CloAllCodB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}
