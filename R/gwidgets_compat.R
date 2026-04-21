# gWidgets2 compatibility layer for RGtk4
# Provides similar API to make conversion easier

#' Extract actual GTK widget from wrapper object
get_widget <- function(obj) {
  if (is.null(obj)) {
    return(NULL)
  } else if (is.list(obj) && !is.null(obj$widget)) {
    return(obj$widget)
  } else {
    return(obj)
  }
}

#' Create a window
gwindow <- function(title = "", parent = NULL, width = 400, height = 300,
                    visible = TRUE, handler = NULL) {
  window <- gtkWindowNew()
  gtkWindowSetTitle(window, title)
  w <- tryCatch(as.numeric(width),  error=function(e) NA)
  h <- tryCatch(as.numeric(height), error=function(e) NA)
  gtkWindowSetDefaultSize(window,
                          as.integer(if (length(w)==0 || is.na(w) || w <= 0) 600L else w),
                          as.integer(if (length(h)==0 || is.na(h) || h <= 0) 400L else h))



  # Cmd+W / Ctrl+W closes this window
  win_key <- gtkEventControllerKeyNew()
  gSignalConnectR(win_key, "key-pressed", function(ctrl, keyval, keycode, state) {
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error = function(e) "")
    if (ch == "w" || ch == "W") { gtkWindowClose(window); return(TRUE) }
    FALSE
  })
  gtkWidgetAddController(window, win_key)

  # GTK4: Properly track window for application
  # This ensures the window appears in taskbar/dock
  tryCatch({
    # Mark window as a toplevel application window
    gtkWindowSetDecorated(window, TRUE)
    gtkWindowSetDeletable(window, TRUE)
    gtkWindowSetResizable(window, TRUE)
  }, error = function(e) {
    # These functions may not all exist, continue anyway
  })

  # Don't skip taskbar - we want it to show in dock/taskbar
  # Note: These functions may not exist in GTK4
  tryCatch({
    gtkWindowSetSkipTaskbarHint(window, FALSE)
  }, error = function(e) {})

  tryCatch({
    gtkWindowSetSkipPagerHint(window, FALSE)
  }, error = function(e) {})

  # Connect destroy handler if provided
  if (!is.null(handler)) {
    gSignalConnectR(window, "close-request", function(w) {
      handler(list(obj = window))
      FALSE  # Allow close
    })
  }

  # Set visibility
  if (visible) {
    gtkWindowPresent(window)
  }

  # Return window with helper methods
  structure(list(
    widget = window,
    set_icon = function(filename) {
      # GTK4 doesn't use set_icon_from_file the same way
      # We'll skip for now or implement later
      invisible(NULL)
    }
  ), class = "gwindow")
}

#' Create a notebook (tabbed interface)
gnotebook <- function(tab.pos = 3, container = NULL, closebuttons = FALSE, ...) {
  notebook <- gtkNotebookNew()

  # Set tab position (1=left, 2=right, 3=top, 4=bottom)
  # GTK4 uses GtkPositionType: LEFT=0, RIGHT=1, TOP=2, BOTTOM=3
  pos_map <- c(0L, 1L, 2L, 3L)  # Map from old to new
  if (tab.pos >= 1 && tab.pos <= 4) {
    gtkNotebookSetTabPos(notebook, pos_map[tab.pos])
  }

  # Add to container if provided
  if (!is.null(container)) {
    if (inherits(container, "gwindow")) {
      gtkWindowSetChild(container$widget, notebook)
    } else if (inherits(container, "ggroup")) {
      gtkBoxAppend(container$widget, notebook)
    } else {
      gtkBoxAppend(container, notebook)
    }
  }

  structure(list(widget = notebook), class = "gnotebook")
}

#' Create a box container (group)
ggroup <- function(horizontal = TRUE, container = NULL, label = NULL, ...) {
  # GTK4: 0 = horizontal, 1 = vertical
  orientation <- if (horizontal) 0L else 1L
  box <- gtkBoxNew(orientation, 5L)  # 5px spacing

  # Add to container if provided
  if (!is.null(container)) {
    parent_widget <- get_widget(container)

    if (inherits(container, "gnotebook")) {
      # Add as notebook page
      tab_label <- gtkLabelNew(if (!is.null(label)) gsub("\n","",label) else "Tab")
      gtkLabelSetXalign(tab_label, 0.5)
      gtkNotebookAppendPage(parent_widget, box, tab_label)
    } else if (inherits(container, "gwindow")) {
      gtkWindowSetChild(parent_widget, box)
    } else if (inherits(container, "ggroup")) {
      gtkBoxAppend(parent_widget, box)
    } else if (inherits(container, "gpanedgroup")) {
      if (is.null(container$widget)) {
        warning("gpanedgroup has NULL widget")
      } else if (horizontal) {
        # Horizontal = button bar: goes into btn_vbox above the pane
        gtkWidgetSetVexpand(box, FALSE)
        gtkBoxAppend(container$btn_vbox, box)
      } else if (!is.null(container$state)) {
        if (!container$state$first_child_set) {
          tryCatch({
            gtkPanedSetStartChild(container$widget, box)
            container$state$first_child_set <- TRUE
          }, error = function(e) warning("Failed paned start child: ", e$message))
        } else {
          tryCatch(
            gtkPanedSetEndChild(container$widget, box),
            error = function(e) warning("Failed paned end child: ", e$message))
        }
      }
    } else {
      # Fallback - try to append to box
      tryCatch({
        gtkBoxAppend(parent_widget, box)
      }, error = function(e) {
        warning("Could not add to container: ", e$message)
      })
    }
  }

  structure(list(widget = box, label = label, horizontal = horizontal), class = "ggroup")
}

#' Create a paned group
gpanedgroup <- function(horizontal = TRUE, container = NULL, label = NULL, ...) {
  orientation <- if (horizontal) 0L else 1L
  paned <- tryCatch(gtkPanedNew(orientation), error = function(e) NULL)

  if (is.null(paned)) {
    warning("GtkPaned creation failed")
    return(structure(list(widget=NULL, btn_vbox=gtkBoxNew(1L,0L),
                          state=new.env()), class="gpanedgroup"))
  }

  # btn_vbox holds button bars (added first, stays at top)
  # outer_vbox = btn_vbox + paned
  btn_vbox   <- gtkBoxNew(1L, 0L)
  outer_vbox <- gtkBoxNew(1L, 0L)
  sep <- gtkSeparatorNew(0L)  # horizontal separator
  gtkWidgetSetMarginTop(btn_vbox, 4L)
  gtkWidgetSetMarginStart(btn_vbox, 2L)
  gtkWidgetSetVexpand(btn_vbox, FALSE)
  gtkWidgetSetVexpand(sep, FALSE)
  gtkWidgetSetVexpand(paned, TRUE)
  gtkBoxAppend(outer_vbox, btn_vbox)
  gtkBoxAppend(outer_vbox, sep)
  gtkBoxAppend(outer_vbox, paned)

  if (!is.null(container) && inherits(container, "gnotebook")) {
    tab_label <- gtkLabelNew(if (!is.null(label)) gsub("\n","",label) else "Tab")
    gtkLabelSetXalign(tab_label, 0.5)
    tryCatch(
      gtkNotebookAppendPage(container$widget, outer_vbox, tab_label),
      error = function(e) warning("Failed to add page: ", e$message))
  }

  state_env <- new.env(parent = emptyenv())
  state_env$first_child_set <- FALSE

  structure(list(
    widget     = paned,
    btn_vbox   = btn_vbox,
    outer_vbox = outer_vbox,
    state      = state_env
  ), class = "gpanedgroup")
}


#' Create a button
gbutton <- function(text = "", container = NULL, handler = NULL, ...) {
  button <- gtkButtonNew()
  gtkButtonSetLabel(button, text)

  # Connect click handler
  if (!is.null(handler)) {
    gSignalConnectR(button, "clicked", function(w) {
      handler(list(obj = button))
    })
  }

  # Add to container
  if (!is.null(container)) {
    parent_widget <- get_widget(container)

    if (inherits(container, "glayout")) {
      # Will be handled by layout indexing
    } else {
      tryCatch({
        # In horizontal containers, don't let buttons grow vertically
        if (inherits(container, "ggroup") && isTRUE(container$horizontal)) {
          gtkWidgetSetVexpand(button, FALSE)
          gtkWidgetSetValign(button, 3L)  # GTK_ALIGN_CENTER
        }
        gtkBoxAppend(parent_widget, button)
      }, error = function(e) {
        warning("Could not add button to container: ", e$message)
      })
    }
  }

  structure(list(widget = button), class = "gbutton")
}

#' Create a label
glabel <- function(text = "", container = NULL, handler = NULL, ...) {
  label <- gtkLabelNew(text)
  gtkLabelSetXalign(label, 0.0)  # Left align

  # If handler provided, make it clickable
  if (!is.null(handler)) {
    # Wrap in a button for clickability
    button <- gtkButtonNew()
    gtkButtonSetChild(button, label)
    gtkButtonSetHasFrame(button, FALSE)  # Make it look like just text

    gSignalConnectR(button, "clicked", function(w) {
      handler(list(obj = label))
    })

    widget_to_add <- button
  } else {
    widget_to_add <- label
  }

  # Add to container
  if (!is.null(container)) {
    if (inherits(container, "ggroup")) {
      gtkBoxAppend(container$widget, widget_to_add)
    } else {
      gtkBoxAppend(get_widget(container), widget_to_add)
    }
  }

  structure(list(widget = label, button = if (!is.null(handler)) button else NULL),
            class = "glabel")
}

#' Create a separator
gseparator <- function(horizontal = TRUE, container = NULL, ...) {
  # GTK4: GtkSeparator
  orientation <- if (horizontal) 0L else 1L
  sep <- gtkSeparatorNew(orientation)

  if (!is.null(container)) {
    if (inherits(container, "ggroup")) {
      gtkBoxAppend(container$widget, sep)
    } else {
      gtkBoxAppend(get_widget(container), sep)
    }
  }

  structure(list(widget = sep), class = "gseparator")
}

#' Create a table/list widget
gtable <- function(items = character(0), container = NULL, multiple = FALSE, handler = NULL, ...) {
  # For now, use a simple GtkListBox
  # TODO: Properly implement with GtkColumnView for full table support
  scrolled <- gtkScrolledWindowNew()
  listbox <- gtkListBoxNew()
  gtkScrolledWindowSetChild(scrolled, listbox)

  # Set selection mode
  if (multiple) {
    gtkListBoxSetSelectionMode(listbox, 2L)  # GTK_SELECTION_MULTIPLE
  } else {
    gtkListBoxSetSelectionMode(listbox, 1L)  # GTK_SELECTION_SINGLE
  }

  # Make listbox activate on single click
  tryCatch({
    gtkListBoxSetActivateOnSingleClick(listbox, TRUE)
  }, error = function(e) {})

  # Placeholder shown when list is empty
  placeholder_lbl <- gtkLabelNew("")
  gtkLabelSetMarkup(placeholder_lbl,
                    "<span foreground='gray' style='italic'>No items</span>")
  gtkWidgetSetMarginTop(placeholder_lbl, 20L)
  gtkWidgetSetMarginBottom(placeholder_lbl, 20L)
  tryCatch(gtkListBoxSetPlaceholder(listbox, placeholder_lbl), error=function(e){})

  # Add click gesture for multiple-selection widgets (row-activated unreliable there)
  # and row-activated for single-selection widgets
  if (!is.null(handler)) {
    if (multiple) {
      # For multiple selection: use click gesture on scrolled window
      click_g <- gtkGestureClickNew()
      gSignalConnectR(click_g, "released", function(g, n_press, x, y) {
        tryCatch({
          adj  <- gtkScrolledWindowGetVadjustment(scrolled)
          yoff <- gtkAdjustmentGetValue(adj)
          coords <- gtkWidgetTranslateCoordinates(scrolled, listbox,
                                                  as.integer(x), as.integer(y))
          ly  <- if (!is.null(coords)) coords$dest_y else as.integer(y + yoff)
          row <- gtkListBoxGetRowAtY(listbox, as.integer(ly))
          if (is.null(row)) return()
          hbox <- gtkListBoxRowGetChild(row)
          text <- NULL
          child <- gtkWidgetGetFirstChild(hbox)
          while (!is.null(child)) {
            t <- tryCatch(gtkLabelGetText(child), error=function(e) NULL)
            if (!is.null(t) && nzchar(t)) text <- t
            child <- gtkWidgetGetNextSibling(child)
          }
          if (!is.null(text) && nzchar(text)) handler(list(obj=list(value=text)))
        }, error=function(e){})
      })
      gtkWidgetAddController(scrolled, click_g)
    }
    tryCatch({
      gSignalConnectR(listbox, "row-activated", function(box, row) {
        # row -> hbox -> [swatch?] -> text_label (last child)
        hbox <- gtkListBoxRowGetChild(row)
        if (is.null(hbox)) return()
        text <- NULL
        child <- gtkWidgetGetFirstChild(hbox)
        while (!is.null(child)) {
          t <- tryCatch(gtkLabelGetText(child), error = function(e) NULL)
          if (!is.null(t) && nzchar(t)) text <- t
          child <- gtkWidgetGetNextSibling(child)
        }
        if (!is.null(text) && nzchar(text)) handler(list(obj = list(value = text)))
      })
    }, error = function(e) {
      message("Could not connect row-activated signal: ", e$message)
    })
  }

  # Add items
  for (item in items) {
    row <- gtkListBoxRowNew()

    # Create a horizontal box for icon + label
    hbox <- gtkBoxNew(0L, 6L)  # horizontal, 6px spacing

    # Add a small icon (file icon)
    # Using Unicode character for now, could be replaced with actual icon
    icon_label <- gtkLabelNew("📄")
    gtkBoxAppend(hbox, icon_label)

    # Add the text label
    label <- gtkLabelNew(item)

    # Make label left-aligned and take full width
    gtkLabelSetXalign(label, 0.0)
    gtkWidgetSetHexpand(label, TRUE)

    # Make text larger and add padding
    tryCatch({
      # Increase font size slightly
      attr_list <- pangoAttrListNew()
      attr_scale <- pangoAttrScaleNew(1.1)  # 10% larger
      pangoAttrListInsert(attr_list, attr_scale)
      gtkLabelSetAttributes(label, attr_list)
    }, error = function(e) {
      # Pango functions might not work, continue without
    })

    gtkBoxAppend(hbox, label)

    # Add some padding to the row
    gtkWidgetSetMarginTop(hbox, 8L)
    gtkWidgetSetMarginBottom(hbox, 8L)
    gtkWidgetSetMarginStart(hbox, 12L)
    gtkWidgetSetMarginEnd(hbox, 12L)

    gtkListBoxRowSetChild(row, hbox)

    # Make row activatable and selectable
    tryCatch({
      gtkListBoxRowSetActivatable(row, TRUE)
      gtkListBoxRowSetSelectable(row, TRUE)
    }, error = function(e) {
      # Functions might not exist
    })

    gtkListBoxAppend(listbox, row)
  }

  # Add to container
  if (!is.null(container)) {
    if (inherits(container, "ggroup")) {
      gtkBoxAppend(container$widget, scrolled)
    } else if (inherits(container, "gpanedgroup")) {
      # Check for valid widget
      if (is.null(container$widget)) {
        warning("gpanedgroup has NULL widget, cannot add table")
      } else if (!is.null(container$state)) {
        # Use environment-based state tracking
        if (!container$state$first_child_set) {
          gtkPanedSetStartChild(container$widget, scrolled)
          container$state$first_child_set <- TRUE
        } else {
          gtkPanedSetEndChild(container$widget, scrolled)
        }
      } else {
        warning("gpanedgroup missing state environment")
      }
    } else {
      gtkBoxAppend(get_widget(container), scrolled)
    }
  }

  obj <- structure(list(
    widget = scrolled,
    listbox = listbox,
    items = items,
    selection_mode = if(multiple) 2L else 1L # Store the mode as an R integer
  ), class = "gtable")

  # Add methods
  obj
}

#' Set names for a widget
#' @export
`names<-.gtable` <- function(x, value) {
  # Set the title/label for the table
  # In GTK4, we might add a label above the scrolled window
  # For now, just store it
  attr(x, "column_name") <- value
  x
}

#' Create a layout container (grid)
glayout <- function(homogeneous = FALSE, spacing = 5, container = NULL, ...) {

  # Create grid with error handling
  grid <- tryCatch({
    g <- gtkGridNew()
    gtkGridSetRowSpacing(g, as.integer(spacing))
    gtkGridSetColumnSpacing(g, as.integer(spacing))

    if (homogeneous) {
      gtkGridSetRowHomogeneous(g, TRUE)
      gtkGridSetColumnHomogeneous(g, TRUE)
    }

    g
  }, error = function(e) {
    warning("Failed to create GtkGrid: ", e$message)
    return(NULL)
  })

  if (is.null(grid)) {
    warning("Grid creation failed, returning dummy object")
    # Return dummy object to prevent crashes
    return(structure(list(widget = NULL, next_row = 0, next_col = 0), class = "glayout"))
  }

  # Add to container with improved error handling
  if (!is.null(container)) {
    if (inherits(container, "ggroup")) {
      tryCatch({
        gtkBoxAppend(container$widget, grid)
      }, error = function(e) {
        warning("Failed to append grid to ggroup: ", e$message)
      })
    } else if (inherits(container, "gpanedgroup")) {
      # glayout (button grid) goes into btn_vbox above the separator
      gtkWidgetSetVexpand(grid, FALSE)
      if (!is.null(container$btn_vbox))
        gtkBoxAppend(container$btn_vbox, grid)
    } else if (inherits(container, "gwindow")) {
      tryCatch({
        gtkWindowSetChild(container$widget, grid)
      }, error = function(e) {
        warning("Failed to set grid as window child: ", e$message)
      })
    } else {
      # Try generic box append
      parent_widget <- get_widget(container)
      if (!is.null(parent_widget)) {
        tryCatch({
          gtkBoxAppend(parent_widget, grid)
        }, error = function(e) {
          warning("Failed to append grid to container: ", e$message)
        })
      }
    }
  }

  structure(list(
    widget = grid,
    next_row = 0,
    next_col = 0
  ), class = "glayout")
}

#' Layout indexing
#' @export
`[<-.glayout` <- function(x, i, j, value) {
  if (is.null(x) || is.null(x$widget)) {
    warning("glayout object is NULL or has no widget")
    return(x)
  }

  row <- i - 1  # Convert to 0-indexed
  col <- j - 1

  widget_to_add <- if (inherits(value, c("gbutton", "glabel", "ggroup"))) {
    value$widget
  } else {
    value
  }

  if (is.null(widget_to_add)) {
    warning("Widget to add is NULL")
    return(x)
  }

  tryCatch({
    gtkGridAttach(x$widget, widget_to_add, as.integer(col), as.integer(row), 1L, 1L)
  }, error = function(e) {
    warning("Error attaching widget to grid: ", e$message)
  })

  x
}

#' Create text view
gtext <- function(text = "", container = NULL, ...) {
  scrolled <- gtkScrolledWindowNew()
  textview  <- gtkTextViewNew()
  gtkScrolledWindowSetChild(scrolled, textview)
  gtkTextViewSetWrapMode(textview, 2L)   # GTK_WRAP_WORD
  gtkWidgetSetVexpand(scrolled, TRUE)
  gtkWidgetSetHexpand(scrolled, TRUE)
  gtkWidgetSetMarginTop(textview, 6L)
  gtkWidgetSetMarginBottom(textview, 6L)
  gtkWidgetSetMarginStart(textview, 8L)
  gtkWidgetSetMarginEnd(textview, 8L)
  tryCatch(apply_font_css(textview), error = function(e) {})

  if (length(text) > 0 && !is.na(text) && nzchar(text)) {
    buffer <- gtkTextViewGetBuffer(textview)
    gtkTextBufferSetText(buffer, as.character(text), -1L)
  }

  if (!is.null(container)) {
    if (inherits(container, "gwindow")) {
      gtkWindowSetChild(container$widget, scrolled)
    } else if (inherits(container, "ggroup")) {
      gtkBoxAppend(container$widget, scrolled)
    } else if (inherits(container, "gpanedgroup")) {
      tryCatch({
        if (!container$state$first_child_set) {
          gtkPanedSetStartChild(container$widget, scrolled)
          container$state$first_child_set <- TRUE
        } else {
          gtkPanedSetEndChild(container$widget, scrolled)
        }
      }, error = function(e) {})
    }
  }

  structure(list(widget = scrolled, textview = textview), class = "gtext")
}

#' Get/set size of widget
size <- function(obj) {
  # Return dummy size for now
  c(width = 400, height = 300)
}

`size<-` <- function(obj, value) {
  # Set size request
  if (inherits(obj, c("ggroup", "gtable", "gbutton", "glabel"))) {
    widget <- obj$widget
  } else {
    widget <- obj
  }

  if (length(value) == 2) {
    gtkWidgetSetSizeRequest(widget, as.integer(value[1]), as.integer(value[2]))
  }

  obj
}

#' Dispose/destroy widget
dispose <- function(obj) {
  # In GTK4, windows are destroyed when closed
  if (inherits(obj, "gwindow")) {
    # Window will close naturally
    invisible(NULL)
  }
}

#' Get toolkit widget
getToolkitWidget <- function(obj) {
  if (inherits(obj, c("gbutton", "glabel", "ggroup", "gtable"))) {
    return(obj$widget)
  }
  obj
}

#' Add keystroke handler
addHandlerKeystroke <- function(obj, handler) {
  # Add key press event to window
  if (inherits(obj, "gwindow")) {
    controller <- gtkEventControllerKeyNew()
    gSignalConnectR(controller, "key-pressed", function(widget, keyval, keycode, state) {
      # Convert keyval to character
      # \021 is Ctrl+Q
      key_char <- rawToChar(as.raw(keyval))
      handler(list(key = key_char))
      FALSE
    })
    gtkWidgetAddController(obj$widget, controller)
  }
}

#' File chooser dialog
gfile <- function(text = "Select a file", type = c("open", "save", "selectdir"),
                  filter = NULL, multi = FALSE, ...) {
  type <- match.arg(type)

  action <- switch(type,
                   open = 0L,      # GTK_FILE_CHOOSER_ACTION_OPEN
                   save = 1L,      # GTK_FILE_CHOOSER_ACTION_SAVE
                   selectdir = 2L  # GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
  )

  tryCatch({
    # Create GtkFileChooserNative
    dialog <- gtkFileChooserNativeNew(
      text,
      NULL,  # parent window - TODO: pass actual parent
      action,
      if (type == "save") "_Save" else "_Open",
      "_Cancel"
    )

    # Set multiple selection if needed
    if (multi) {
      gtkFileChooserSetSelectMultiple(dialog, TRUE)
    }

    # Add filters if provided
    if (!is.null(filter)) {
      tryCatch({
        for (filter_name in names(filter)) {
          file_filter <- gtkFileFilterNew()
          gtkFileFilterSetName(file_filter, filter_name)

          patterns <- filter[[filter_name]]$patterns
          if (!is.null(patterns)) {
            for (pattern in patterns) {
              gtkFileFilterAddPattern(file_filter, pattern)
            }
          }

          gtkFileChooserAddFilter(dialog, file_filter)
        }
      }, error = function(e) {
        message("Could not add file filters: ", e$message)
      })
    }

    # Use signal-based approach (blocking run doesn't exist in bindings)
    result <- character(0)
    done <- FALSE

    # Connect response handler
    gSignalConnectR(dialog, "response", function(dlg, response_id) {
      if (response_id == -3L) {  # GTK_RESPONSE_ACCEPT
        tryCatch({
          if (multi) {
            # TODO: Get multiple files - for now just get single
            gfile_obj <- gtkFileChooserGetFile(dlg)
            if (!is.null(gfile_obj)) {
              path <- gFileGetPath(gfile_obj)
              result <<- if (!is.null(path)) path else character(0)
            }
          } else {
            gfile_obj <- gtkFileChooserGetFile(dlg)
            if (!is.null(gfile_obj)) {
              path <- gFileGetPath(gfile_obj)
              result <<- if (!is.null(path)) path else character(0)
            }
          }
        }, error = function(e) {
          message("Error getting file: ", e$message)
        })
      }
      done <<- TRUE
    })

    # Show dialog
    gtkNativeDialogShow(dialog)

    # Wait for response (manual event loop)
    timeout <- 0
    while (!done && timeout < 30000) {  # 30 second timeout
      gMainContextIteration(NULL, FALSE)
      Sys.sleep(0.01)
      timeout <- timeout + 1
    }

    if (timeout >= 30000) {
      message("File dialog timed out")
    }

    return(result)

  }, error = function(e) {
    # Fallback to simple text input if GTK dialogs fail
    message("File dialog error: ", e$message)
    message("Falling back to text input...")
    result <- readline(paste0(text, "\nEnter file path: "))
    if (result == "") character(0) else result
  })
}

#' Set/get value of a widget
svalue <- function(obj) {
  if (inherits(obj, "glabel")) {
    gtkLabelGetText(obj$widget)
  } else if (inherits(obj, "gtext")) {
    tryCatch({
      buf <- gtkTextViewGetBuffer(obj$textview)
      si  <- gtkTextBufferGetStartIter(buf)
      ei  <- gtkTextBufferGetEndIter(buf)
      raw <- gtkTextBufferGetText(buf, si, ei, FALSE)
      if (is.list(raw)) raw <- raw[[1]]
      as.character(raw)
    }, error = function(e) "")
  } else if (inherits(obj, "gtable")) {
    # Safety check - don't call GTK functions on invalid widgets
    tryCatch({
      if (is.null(obj$listbox)) {
        warning("gtable has NULL listbox")
        return(character(0))
      }

      # Validate the widget pointer is valid before calling GTK functions
      # If listbox has no items, return empty
      if (is.null(obj$items) || length(obj$items) == 0) {
        return(character(0))
      }

      # Try to get selection - this can segfault if widget is invalid
      # So we wrap it carefully and have fallbacks
      selected_row <- NULL
      tryCatch({
        selected_row <- gtkListBoxGetSelectedRow(obj$listbox)
      }, error = function(e) {
        # GTK function failed, return nothing
        NULL
      })

      if (is.null(selected_row)) {
        # No selection or error getting selection
        return(character(0))
      }

      # Row structure: row -> hbox -> [optional swatch] -> text_label
      # Text is always the last child of hbox
      hbox <- gtkListBoxRowGetChild(selected_row)
      if (!is.null(hbox)) {
        tryCatch({
          result <- NULL
          child <- gtkWidgetGetFirstChild(hbox)
          while (!is.null(child)) {
            t <- tryCatch(gtkLabelGetText(child), error = function(e) NULL)
            if (!is.null(t) && nzchar(t)) result <- t
            child <- gtkWidgetGetNextSibling(child)
          }
          if (!is.null(result)) return(result)
        }, error = function(e) {})
      }

      character(0)
    }, error = function(e) {
      # Complete failure - return empty
      character(0)
    })
  } else {
    NULL
  }
}

`svalue<-` <- function(obj, value) {
  if (inherits(obj, "glabel")) {
    if (!is.null(obj$button)) {
      # It's wrapped in a button, get the label child
      label <- gtkButtonGetChild(obj$button)
      gtkLabelSetText(label, value)
    } else {
      gtkLabelSetText(obj$widget, value)
    }
  } else if (inherits(obj, "gtable")) {
    # TEMPORARY: Just add new items without clearing to prevent crash
    # The clearing was causing infinite GTK warnings
    listbox <- obj$listbox

    # Add new items
    if (!is.null(value) && length(value) > 0) {
      for (item in value) {
        row <- gtkListBoxRowNew()

        # Create horizontal box for icon + label
        hbox <- gtkBoxNew(0L, 6L)

        # Add icon
        icon_label <- gtkLabelNew("📄")
        gtkBoxAppend(hbox, icon_label)

        # Add text label
        label <- gtkLabelNew(as.character(item))

        # Make label left-aligned and take full width
        gtkLabelSetXalign(label, 0.0)
        gtkWidgetSetHexpand(label, TRUE)

        # Make text larger
        tryCatch({
          attr_list <- pangoAttrListNew()
          attr_scale <- pangoAttrScaleNew(1.1)
          pangoAttrListInsert(attr_list, attr_scale)
          gtkLabelSetAttributes(label, attr_list)
        }, error = function(e) {})

        gtkBoxAppend(hbox, label)

        # Add padding
        gtkWidgetSetMarginTop(hbox, 8L)
        gtkWidgetSetMarginBottom(hbox, 8L)
        gtkWidgetSetMarginStart(hbox, 12L)
        gtkWidgetSetMarginEnd(hbox, 12L)

        gtkListBoxRowSetChild(row, hbox)

        # Make row activatable and selectable
        tryCatch({
          gtkListBoxRowSetActivatable(row, TRUE)
          gtkListBoxRowSetSelectable(row, TRUE)
        }, error = function(e) {})

        gtkListBoxAppend(listbox, row)
      }
    }
  }

  obj
}

#' Enable/disable widget
enabled <- function(obj) {
  if (inherits(obj, c("gbutton", "glabel", "gtable", "ggroup"))) {
    gtkWidgetGetSensitive(obj$widget)
  } else {
    TRUE
  }
}

`enabled<-` <- function(obj, value) {
  if (inherits(obj, c("gbutton", "glabel", "gtable", "ggroup"))) {
    if (!is.null(obj$widget))
      tryCatch(gtkWidgetSetSensitive(obj$widget, as.logical(value)), error=function(e){})
  }
  obj
}

#' Get screen dimensions (approximation)
gdkScreenWidth <- function() {
  # Return sensible default, GTK4 doesn't have direct screen query
  1920
}

gdkScreenHeight <- function() {
  1080
}

#' Table/list item access
#' @export
`[.gtable` <- function(x, i) {
  # Get items from table
  x$items[i]
}

#' @export
# Build a single listbox row with optional leading color swatch
.make_listbox_row <- function(item, color = NULL, prefix = NULL) {
  row  <- gtkListBoxRowNew()
  hbox <- gtkBoxNew(0L, 6L)

  if (!is.null(color) && !is.na(color) && nzchar(color)) {
    # Color swatch using Pango markup on a label
    swatch_lbl <- gtkLabelNew("")
    markup <- sprintf('<span foreground="%s">&#x25A0;</span>', color)
    tryCatch(gtkLabelSetMarkup(swatch_lbl, markup),
             error = function(e) gtkLabelSetText(swatch_lbl, "■"))
    gtkBoxAppend(hbox, swatch_lbl)
  } else if (!is.null(prefix)) {
    pfx <- gtkLabelNew(prefix)
    gtkBoxAppend(hbox, pfx)
  }

  lbl <- gtkLabelNew(as.character(item))
  gtkLabelSetXalign(lbl, 0.0)
  gtkWidgetSetHexpand(lbl, TRUE)
  gtkBoxAppend(hbox, lbl)

  gtkWidgetSetMarginTop(hbox, 6L)
  gtkWidgetSetMarginBottom(hbox, 6L)
  gtkWidgetSetMarginStart(hbox, 10L)
  gtkWidgetSetMarginEnd(hbox, 10L)

  gtkListBoxRowSetChild(row, hbox)
  row
}

#' @export
"[<-.gtable" <- function(x, i, value, colors = NULL) {
  if (is.null(x) || is.null(x$widget)) {
    warning("gtable object is NULL or has no widget")
    return(x)
  }

  if (missing(i)) {
    lb <- x$listbox
    # Clear existing rows
    tryCatch({
      repeat {
        row <- gtkListBoxGetRowAtIndex(lb, 0L)
        if (is.null(row)) break
        gtkListBoxRemove(lb, row)
      }
    }, error = function(e) {})

    if (!is.null(value) && length(value) > 0) {
      for (idx in seq_along(value)) {
        col <- if (!is.null(colors) && idx <= length(colors)) colors[[idx]] else NULL
        row <- .make_listbox_row(value[[idx]], color = col)
        gtkListBoxAppend(lb, row)
      }
      x$items <- value
    } else {
      x$items <- character(0)
    }
    # listbox ref unchanged - no swap needed
  }

  return(x)
}

# Connect a row-activated handler to the stable scrolled window via click gesture.
# The scrolled window never changes even when [<- swaps the listbox, so this
# handler survives all updates. obj is the gtable; the listbox ref inside is
# read at click time via the closure over obj$widget (scrolled).
add_row_activated <- function(obj, handler) {
  if (!inherits(obj, "gtable")) return(invisible(NULL))
  scrolled <- obj$widget
  gesture <- gtkGestureClickNew()
  gSignalConnectR(gesture, "released", function(g, n_press, x, y) {
    handler(list())
  })
  gtkWidgetAddController(scrolled, gesture)
  invisible(obj)
}

addHandlerClicked <- function(obj, handler) {
  if (!inherits(obj, "gtable")) return(invisible(NULL))
  lb <- obj$listbox
  if (is.null(lb)) return(invisible(NULL))
  tryCatch(
    gSignalConnectR(lb, "row-activated", function(box, row) handler(list(obj = obj))),
    error = function(e) message("addHandlerClicked: ", e$message)
  )
  invisible(NULL)
}

addHandlerDoubleclick <- function(obj, handler) {
  invisible(NULL)
}


#' Multi-select list dialog — blocks until OK/Cancel, returns character vector or NULL
#' @export
gselect_multi <- function(items, title = "Select", message = NULL) {
  if (length(items) == 0) {
    gmessage("Nothing available to select.", icon = "info")
    return(NULL)
  }

  res  <- list(val = NULL)
  loop <- gMainLoopNew(NULL, FALSE)

  dialog <- gtkWindowNew()
  gtkWindowSetTitle(dialog, title)
  gtkWindowSetDefaultSize(dialog, 300L, 400L)
  gtkWindowSetModal(dialog, TRUE)
  if (exists(".rqda_window", envir = .rqda))
    gtkWindowSetTransientFor(dialog, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 8L)
  gtkWidgetSetMarginTop(vbox, 12L); gtkWidgetSetMarginBottom(vbox, 12L)
  gtkWidgetSetMarginStart(vbox, 12L); gtkWidgetSetMarginEnd(vbox, 12L)
  gtkWindowSetChild(dialog, vbox)

  if (!is.null(message)) {
    lbl <- gtkLabelNew(message)
    gtkLabelSetXalign(lbl, 0.0)
    gtkLabelSetWrap(lbl, TRUE)
    gtkBoxAppend(vbox, lbl)
  }

  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)
  gtkBoxAppend(vbox, sw)

  listbox <- gtkListBoxNew()
  gtkListBoxSetSelectionMode(listbox, 1L)  # GTK_SELECTION_SINGLE - MULTIPLE has API issues
  gtkScrolledWindowSetChild(sw, listbox)

  for (item in items) {
    row <- gtkListBoxRowNew()
    lbl <- gtkLabelNew(as.character(item))
    gtkLabelSetXalign(lbl, 0.0)
    gtkWidgetSetMarginTop(lbl, 4L); gtkWidgetSetMarginBottom(lbl, 4L)
    gtkWidgetSetMarginStart(lbl, 8L); gtkWidgetSetMarginEnd(lbl, 8L)
    gtkListBoxRowSetChild(row, lbl)
    gtkListBoxAppend(listbox, row)
  }

  bbox <- gtkBoxNew(0L, 8L)
  gtkWidgetSetHalign(bbox, 3L)
  gtkBoxAppend(vbox, bbox)

  on_ok <- function(...) {
    row <- tryCatch(gtkListBoxGetSelectedRow(listbox), error = function(e) NULL)
    if (!is.null(row)) {
      lbl <- tryCatch(gtkListBoxRowGetChild(row), error = function(e) NULL)
      txt <- tryCatch(gtkLabelGetText(lbl), error = function(e) NULL)
      res$val <<- if (!is.null(txt) && nzchar(txt)) txt else character(0)
    } else {
      res$val <<- character(0)
    }
    gMainLoopQuit(loop)
    gtkWindowDestroy(dialog)
  }

  on_cancel <- function(...) {
    res$val <<- NULL
    gMainLoopQuit(loop)
    gtkWindowDestroy(dialog)
  }

  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  ok_btn     <- gtkButtonNewWithLabel("OK")
  gSignalConnectR(cancel_btn, "clicked", on_cancel)
  gSignalConnectR(ok_btn,     "clicked", on_ok)
  gSignalConnectR(dialog, "close-request", function(w) { on_cancel(); FALSE })
  gtkBoxAppend(bbox, cancel_btn)
  gtkBoxAppend(bbox, ok_btn)

  gtkWindowPresent(dialog)
  gMainLoopRun(loop)
  res$val
}

#' GTK4 Input Dialog (ginput)
#' @param message The prompt text for the user
#' @param text Default text to appear in the entry box
#' @param title The window title
#' @param handler A function(val) to execute when the user clicks OK
#' @export
ginput <- function(message, text = "", title = "Input", parent = NULL) {

  # 1. State Management
  # Use a list to distinguish between a Cancel (NULL) and an empty string ("")
  res <- list(val = NULL)
  loop <- gMainLoopNew(NULL, FALSE)

  # 2. Create the Window
  dialog <- gtkWindowNew()
  gtkWindowSetTitle(dialog, as.character(title))
  gtkWindowSetDefaultSize(dialog, 350L, -1L)
  gtkWindowSetModal(dialog, TRUE)

  # Set parent if provided or found in .rqda
  if (!is.null(parent)) {
    gtkWindowSetTransientFor(dialog, parent)
  } else if (exists(".rqda_window", envir = .rqda)) {
    gtkWindowSetTransientFor(dialog, .rqda$.rqda_window)
  }

  # 3. Layout Container
  vbox <- gtkBoxNew(1L, 15L)
  gtkWidgetSetMarginTop(vbox, 15L)
  gtkWidgetSetMarginBottom(vbox, 15L)
  gtkWidgetSetMarginStart(vbox, 15L)
  gtkWidgetSetMarginEnd(vbox, 15L)
  gtkWindowSetChild(dialog, vbox)

  # 4. Label & Entry
  lbl <- gtkLabelNew(as.character(message))
  gtkLabelSetXalign(lbl, 0.0)
  gtkBoxAppend(vbox, lbl)

  entry <- gtkEntryNew()
  buffer <- gtkEntryGetBuffer(entry)
  gtkEntryBufferSetText(buffer, as.character(text), -1L)
  gtkBoxAppend(vbox, entry)

  # 5. Buttons Layout
  bbox <- gtkBoxNew(0L, 10L)
  gtkWidgetSetHalign(bbox, 3L)
  gtkBoxAppend(vbox, bbox)

  # 6. Submission Logic
  on_submit <- function(...) {
    raw_val <- gtkEntryBufferGetText(gtkEntryGetBuffer(entry))
    res$val <<- as.character(raw_val)
    gMainLoopQuit(loop)
    gtkWindowDestroy(dialog)
  }

  on_cancel <- function(...) {
    res$val <<- NULL # Return NULL to indicate cancellation
    gMainLoopQuit(loop)
    gtkWindowDestroy(dialog)
  }

  # 7. Create Buttons
  ok_btn <- gtkButtonNewWithLabel("OK")
  cancel_btn <- gtkButtonNewWithLabel("Cancel")

  # Signals
  gSignalConnectR(ok_btn, "clicked", on_submit)
  gSignalConnectR(entry, "activate", on_submit)
  gSignalConnectR(cancel_btn, "clicked", on_cancel)

  # Ensure the 'X' button also quits the loop
  gSignalConnectR(dialog, "close-request", function(w) {
    on_cancel()
    return(FALSE)
  })

  gtkBoxAppend(bbox, cancel_btn)
  gtkBoxAppend(bbox, ok_btn)

  # 8. Show and Block
  gtkWindowPresent(dialog)

  # Execution halts here until gMainLoopQuit() is called
  gMainLoopRun(loop)

  val <- res$val
  if (is.null(val)) return(NULL)
  val <- tryCatch(as.character(val)[1], error=function(e) NULL)
  if (is.null(val) || is.na(val)) return(NULL)
  return(val)
}

gconfirm <- function(message, title = "Confirm", parent = NULL, icon = "question") {

  # 1. State management
  res <- FALSE
  loop <- gMainLoopNew(NULL, FALSE)

  # 2. Create the Dialog Window
  dialog <- gtkWindowNew()
  gtkWindowSetTitle(dialog, title)
  gtkWindowSetModal(dialog, TRUE)
  if (!is.null(parent)) gtkWindowSetTransientFor(dialog, parent)

  # Set a reasonable minimum width but let height be dynamic
  gtkWindowSetDefaultSize(dialog, 350L, -1L)

  # 3. Main Layout
  main_box <- gtkBoxNew(1L, 15L) # Vertical
  gtkWidgetSetMarginStart(main_box, 20L)
  gtkWidgetSetMarginEnd(main_box, 20L)
  gtkWidgetSetMarginTop(main_box, 20L)
  gtkWidgetSetMarginBottom(main_box, 20L)
  gtkWindowSetChild(dialog, main_box)

  # 4. Content Area (Icon + Text)
  content_box <- gtkBoxNew(0L, 15L) # Horizontal
  gtkBoxAppend(main_box, content_box)

  # Map icon names
  sys_icon <- switch(icon,
                     "question" = "dialog-question",
                     "warning"  = "dialog-warning",
                     "error"    = "dialog-error",
                     "info"     = "dialog-information",
                     "dialog-question")

  image <- gtkImageNewFromIconName(sys_icon)
  gtkWidgetSetSizeRequest(image, 48L, 48L)
  gtkBoxAppend(content_box, image)

  label <- gtkLabelNew(message)
  gtkLabelSetWrap(label, TRUE)
  gtkLabelSetXalign(label, 0.0) # Left align text
  gtkBoxAppend(content_box, label)

  # 5. Action Area (Buttons)
  button_box <- gtkBoxNew(0L, 10L)
  gtkWidgetSetHalign(button_box, 3L) # Align buttons to the right
  gtkBoxAppend(main_box, button_box)

  create_colored_button <- function(label_text) {
    btn <- gtkButtonNew()
    btn_content <- gtkBoxNew(0L, 8L)
    dot_icon <- gtkImageNewFromIconName("media-record-symbolic")
    gtkWidgetSetSizeRequest(dot_icon, 12L, 12L)
    btn_label <- gtkLabelNew(label_text)
    gtkBoxAppend(btn_content, dot_icon)
    gtkBoxAppend(btn_content, btn_label)
    gtkButtonSetChild(btn, btn_content)
    return(list(button = btn, icon = dot_icon))
  }

  yes_item <- create_colored_button("Yes")
  no_item  <- create_colored_button("No")
  gtkBoxAppend(button_box, yes_item$button)
  gtkBoxAppend(button_box, no_item$button)

  # 6. Styling the dots
  css_provider <- gtkCssProviderNew()
  gtkCssProviderLoadFromData(css_provider, "
    .green-dot { color: #228B22; }
    .red-dot { color: #cc0000; }
  ", -1.0)

  # Apply CSS to Yes dot
  ctx_yes <- gtkWidgetGetStyleContext(yes_item$icon)
  gtkStyleContextAddProvider(ctx_yes, css_provider, 800L)
  gtkStyleContextAddClass(ctx_yes, "green-dot")

  # Apply CSS to No dot
  ctx_no <- gtkWidgetGetStyleContext(no_item$icon)
  gtkStyleContextAddProvider(ctx_no, css_provider, 800L)
  gtkStyleContextAddClass(ctx_no, "red-dot")

  # 7. Signals & Blocking Logic
  gSignalConnectR(yes_item$button, "clicked", function(w) {
    res <<- TRUE
    gMainLoopQuit(loop)
    gtkWindowDestroy(dialog)
  })

  gSignalConnectR(no_item$button, "clicked", function(w) {
    res <<- FALSE
    gMainLoopQuit(loop)
    gtkWindowDestroy(dialog)
  })

  # Handle window 'X' button
  gSignalConnectR(dialog, "close-request", function(w) {
    res <<- FALSE
    gMainLoopQuit(loop)
    return(FALSE) # Allow destruction
  })

  # 8. Run
  gtkWindowPresent(dialog)

  # This blocks the R console until gMainLoopQuit is called
  gMainLoopRun(loop)

  return(res)
}
