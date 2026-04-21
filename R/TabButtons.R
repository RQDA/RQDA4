# Real button implementations for category and attribute tabs

# Helper to add button to ggroup container
add_button_to_container <- function(button, container) {
  if (!is.null(container) && inherits(container, "ggroup")) {
    # Validate container has a valid widget
    if (is.null(container$widget)) {
      warning("Container has NULL widget, cannot add button")
      return(button)
    }

    # Validate it's actually a GtkBox
    is_box <- tryCatch({
      gtkBoxGetSpacing(container$widget)
      TRUE
    }, error = function(e) {
      warning("Container widget is not a valid GtkBox: ", e$message)
      FALSE
    })

    if (is_box) {
      tryCatch({
        # In horizontal containers keep buttons compact vertically
        if (isTRUE(container$horizontal)) {
          gtkWidgetSetVexpand(button$widget, FALSE)
          gtkWidgetSetValign(button$widget, 3L)  # GTK_ALIGN_CENTER
        }
        gtkBoxAppend(container$widget, button$widget)
      }, error = function(e) {
        warning("Could not add button to container: ", e$message)
      })
    }
  }
  button
}

### CODE CATEGORY BUTTONS ###

AddCodeCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    catname <- ginput("Enter name for new code category:", title = "Add Code Category")
    print(catname)
    if (length(catname) > 0 && catname != "") {
      tryCatch({
        AddTodbTable(catname, "codecat", Id = "catid")
        UpdateTableWidget(".CodeCatWidget", "codecat")
      }, error = function(e) {
        message("Error adding code category: ", e$message)
      })
    }
  })
  add_button_to_container(btn, container)
}

DeleteCodeCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    selected <- tryCatch(svalue(.rqda$.CodeCatWidget), error = function(e) NULL)
    if (is.null(selected) || length(selected) == 0) {
      gmessage("No category selected", icon = "warning")
      return(invisible(NULL))
    }

    if (gconfirm(paste("Delete category:", selected))) {
      tryCatch({
        rqda_exe(sprintf("UPDATE codecat SET status = 0 WHERE name = '%s'", selected))
        UpdateTableWidget(".CodeCatWidget", "codecat")
        .rqda$.CodeofCat[] <- NULL
      }, error = function(e) {
        message("Error deleting category: ", e$message)
      })
    }
  })
  add_button_to_container(btn, container)
}

AddCodeToCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    tryCatch({
      AddCodeToCategory()
      UpdateCodeofCatWidget()
    }, error = function(e) {
      message("Error adding code to category: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

DropCodeFromCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    tryCatch({
      DropCodeFromCategory()
      UpdateCodeofCatWidget()
    }, error = function(e) {
      message("Error dropping code from category: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

CodeCatMemoButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))
    MemoWidget(rqda_txt("code category"), .rqda$.CodeCatWidget, "codecat")
  })
  add_button_to_container(btn, container)
}

### FILE CATEGORY BUTTONS ###

AddFileCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    catname <- ginput("Enter name for new file category:", title = "Add File Category")
    if (length(catname) > 0 && catname != "") {
      tryCatch({
        AddTodbTable(catname, "filecat", Id = "catid")
        UpdateTableWidget(".FileCatWidget", "filecat")
      }, error = function(e) {
        message("Error adding file category: ", e$message)
      })
    }
  })
  add_button_to_container(btn, container)
}

DeleteFileCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    selected <- tryCatch(svalue(.rqda$.FileCatWidget), error = function(e) NULL)
    if (is.null(selected) || length(selected) == 0) {
      gmessage("No category selected", icon = "warning")
      return(invisible(NULL))
    }

    if (gconfirm(paste("Delete category:", selected))) {
      tryCatch({
        rqda_exe(sprintf("UPDATE filecat SET status = 0 WHERE name = '%s'", selected))
        UpdateTableWidget(".FileCatWidget", "filecat")
        .rqda$.FileofCat[] <- NULL
      }, error = function(e) {
        message("Error deleting category: ", e$message)
      })
    }
  })
  add_button_to_container(btn, container)
}

AddFileToFileCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    tryCatch({
      AddFileToCategory()
      UpdateFileofCatWidget()
    }, error = function(e) {
      message("Error adding file to category: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

DropFileFromFileCatButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    tryCatch({
      DropFileFromCategory()
      UpdateFileofCatWidget()
    }, error = function(e) {
      message("Error dropping file from category: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

FileCatMemoButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))
    MemoWidget(rqda_txt("file category"), .rqda$.FileCatWidget, "filecat")
  })
  add_button_to_container(btn, container)
}

### ATTRIBUTE BUTTONS ###

AddAttrButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    tryCatch({
      AddAttribute()
      AttrNamesUpdate()
    }, error = function(e) {
      message("Error adding attribute: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

DeleteAttrButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    tryCatch({
      DeleteAttribute()
      AttrNamesUpdate()
    }, error = function(e) {
      message("Error deleting attribute: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

SetFileAttrButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))
    tryCatch(SetFileAttribute(svalue(.rqda$.fnames_rqda), ginput("Attribute name:"), ginput("Value:")), error=function(e) message(e$message))
  })
  add_button_to_container(btn, container)
}

FileAttrButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))
    tryCatch(viewFileAttr(), error=function(e) message(e$message))
  })
  add_button_to_container(btn, container)
}

### JOURNAL BUTTONS ###

AddJournalButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))
    tryCatch({
      AddJournal()
      JournalNamesUpdate()
    }, error = function(e) {
      message("Error adding journal: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}

DeleteJournalButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    selected <- tryCatch(svalue(.rqda$.JournalNamesWidget), error = function(e) NULL)
    if (is.null(selected) || length(selected) == 0) {
      gmessage("No journal selected", icon = "warning")
      return(invisible(NULL))
    }

    if (gconfirm(paste("Delete journal:", selected))) {
      tryCatch({
        rqda_exe(sprintf("UPDATE journal SET status = 0 WHERE name = '%s'", selected))
        JournalNamesUpdate()
      }, error = function(e) {
        message("Error deleting journal: ", e$message)
      })
    }
  })
  add_button_to_container(btn, container)
}

OpenJournalButton <- function(label, container = NULL, ...) {
  btn <- gbutton(label, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))

    # Debug: Check if widget exists
    if (!exists(".JournalNamesWidget", envir = .rqda)) {
      return(invisible(NULL))
    }

    # Debug: Check items
    widget <- .rqda$.JournalNamesWidget
    if (!is.null(widget$items)) {
      message("DEBUG: Items are: ", paste(widget$items, collapse = ", "))
    }

    # Try to get selection
    selected <- tryCatch({
      sel <- svalue(.rqda$.JournalNamesWidget)
      sel
    }, error = function(e) {
      message("DEBUG: Error getting selection: ", e$message)
      NULL
    })

    tryCatch({
      OpenJournal()
    }, error = function(e) {
      message("Error opening journal: ", e$message)
    })
  })
  add_button_to_container(btn, container)
}
