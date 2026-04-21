# R/PopupMenu.R - Right-click popup menu for GTK4/macOS

#' Add a right-click popup menu to a gtable widget
#'
#' Uses GMenuModel + GtkPopoverMenu. On macOS, gtkPopoverSetPointingTo crashes
#' due to a GDK monitor assertion bug, so positioning uses gtkPopoverSetOffset
#' relative to the window center. Opens on button release to avoid accidental
#' activation of the first item.
#'
#' @param widget  A gtable object
#' @param menu_spec Named list: names = labels, values = zero-arg callbacks
#' @export
addRightClickMenu <- function(widget, menu_spec, rqda_name = NULL) {
  if (!inherits(widget, "gtable")) return(invisible(NULL))
  if (length(menu_spec) == 0)     return(invisible(NULL))

  scrolled   <- widget$widget  # stable scrolled window

  # Helper to get the current listbox (survives []<- swaps)
  get_listbox <- function() {
    if (!is.null(rqda_name) && exists(rqda_name, envir=.rqda))
      return(.rqda[[rqda_name]]$listbox)
    widget$listbox
  }

  menu_model   <- gMenuNew()
  action_group <- gSimpleActionGroupNew()

  labels    <- names(menu_spec)
  callbacks <- unname(menu_spec)



  win <- tryCatch(gtkWidgetGetRoot(scrolled), error = function(e) NULL)
  if (is.null(win) && exists(".rqda_window", envir = .rqda))
    win <- .rqda$.rqda_window
  if (is.null(win)) return(invisible(NULL))

  # Unique group name so multiple widgets don't overwrite each other
  if (!exists(".popup_counter", envir = .rqda)) assign(".popup_counter", 0L, envir = .rqda)
  .rqda$.popup_counter <- .rqda$.popup_counter + 1L
  grp <- paste0("ctx", .rqda$.popup_counter)

  # Rebuild menu model with correct action prefix
  menu_model <- gMenuNew()
  for (i in seq_along(labels))
    gMenuAppend(menu_model, labels[[i]], paste0(grp, ".item", i))
  for (i in seq_along(labels)) {
    act <- gSimpleActionNew(sprintf("item%d", i), NULL)
    local({ cb <- callbacks[[i]]; gSignalConnectR(act, "activate", function(a, p) cb()) })
    gActionMapAddAction(action_group, act)
  }

  gtkWidgetInsertActionGroup(win, grp, action_group)

  popover <- gtkPopoverMenuNewFromModel(menu_model)
  gtkWidgetSetParent(popover, win)
  gtkPopoverSetHasArrow(popover, FALSE)
  gtkPopoverSetPosition(popover, 3L)  # BOTTOM

  gesture <- gtkGestureClickNew()
  gtkGestureSingleSetButton(gesture, 0L)
  gtkEventControllerSetPropagationPhase(gesture, 1L)  # CAPTURE

  gSignalConnectR(gesture, "released", function(gest, n_press, x, y) {
    event     <- tryCatch(gtkEventControllerGetCurrentEvent(gest), error = function(e) NULL)
    event_btn <- tryCatch(gdkButtonEventGetButton(event), error = function(e) 0L)
    if (!isTRUE(event_btn == 3L)) return()

    # Select the row under the cursor so svalue() works in callbacks
    tryCatch({
      lb   <- get_listbox()
      adj  <- gtkScrolledWindowGetVadjustment(scrolled)
      yoff <- gtkAdjustmentGetValue(adj)
      row  <- gtkListBoxGetRowAtY(lb, as.integer(y + yoff))
      if (!is.null(row)) gtkListBoxSelectRow(lb, row)
    }, error = function(e) {})

    res <- tryCatch(
      gtkWidgetTranslateCoordinates(scrolled, win, as.integer(x), as.integer(y)),
      error = function(e) NULL)
    wx <- if (!is.null(res)) res$dest_x else as.integer(x)
    wy <- if (!is.null(res)) res$dest_y else as.integer(y)

    ww <- gtkWidgetGetWidth(win)
    wh <- gtkWidgetGetHeight(win)

    x_off <- as.integer(wx - ww / 2) + 40L
    y_off <- as.integer(-(wh - wy)) - 10L

    tryCatch(gtkPopoverSetOffset(popover, x_off, y_off), error = function(e) {})
    gtkPopoverPopup(popover)
  })

  gtkWidgetAddController(scrolled, gesture)
  invisible(popover)
}

#' @export
gtkAddPopupMenu <- function(widget, menu_items) {
  spec <- setNames(
    lapply(menu_items, `[[`, "callback"),
    sapply(menu_items, `[[`, "label")
  )
  addRightClickMenu(widget, spec)
}
