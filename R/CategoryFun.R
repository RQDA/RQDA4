# # Category management functions for RQDA

# #' Update table widget from database
# #' @export
# UpdateTableWidget <- function(Widget, FromdbTable, sortByTime = FALSE, decreasing = FALSE) {
#   if (!is_projOpen()) return(invisible(NULL))

#   items <- rqda_sel(sprintf(
#     "SELECT name, date FROM %s WHERE status = 1",
#     FromdbTable
#   ))

#   if (is.null(items) || nrow(items) == 0) {
#     tryCatch(Widget[] <- NULL, error = function(e) {})
#     return(invisible(NULL))
#   }

#   Encoding(items$name) <- "UTF-8"

#   if (!sortByTime) {
#     items_list <- sort(items$name, decreasing = decreasing)
#   } else {
#     items_list <- items$name[OrderByTime(items$date, decreasing = decreasing)]
#   }

#   tryCatch(Widget[] <- items_list, error = function(e) {})

#   invisible(NULL)
# }

# #' Add item to database table
# #' @export
# # AddTodbTable <- function(item, dbTable, Id = "id", field = "name") {

# #   if (item == "") return(invisible(NULL))

# #   # Escape quotes
# #   item <- gsub("'", "''", item)

# #   maxid <- rqda_sel(sprintf("SELECT MAX(%s) FROM %s", Id, dbTable))[[1]]
# #   nextid <- ifelse(is.na(maxid), 1, maxid + 1)

# #   write <- FALSE
# #   if (nextid == 1) {
# #     write <- TRUE
# #   } else {
# #     dup <- rqda_sel(sprintf(
# #       "SELECT %s FROM %s WHERE name='%s'",
# #       field, dbTable, item
# #     ))
# #     if (is.null(dup) || nrow(dup) == 0) {
# #       write <- TRUE
# #     }
# #   }

# #   if (write) {
# #     rqda_exe(sprintf(
# #       "INSERT INTO %s (%s, %s, status, date, owner) VALUES ('%s', %d, 1, '%s', '%s')",
# #       dbTable, field, Id, item, nextid, date(), .rqda$owner
# #     ))
# #   }

# #   invisible(NULL)
# # }

# #' Add Code Category
# #' @export
# AddCodeCat <- function(catname = NULL) {
#   if (!is_projOpen()) return(invisible(NULL))

#   if (is.null(catname)) {
#     catname <- ginput(
#       message = "Enter new Code Category:",
#       text = "",
#       title = "New Code Category"
#     )
#   }

#   if (identical(catname, character(0)) || catname == "") {
#     return(invisible(NULL))
#   }

#   Encoding(catname) <- "UTF-8"
#   AddTodbTable(catname, "codecat", Id = "catid")

#   if (exists(".CodeCatWidget", envir = .rqda)) {
#     UpdateTableWidget(
#       Widget = .rqda$.CodeCatWidget,
#       FromdbTable = "codecat"
#     )
#   }

#   message("Added code category: ", catname)
# }

# #' Delete Code Category
# #' @export
# DeleteCodeCat <- function() {
#   if (!is_projOpen()) return(invisible(NULL))

#   if (!exists(".CodeCatWidget", envir = .rqda)) {
#     gmessage("Code category widget not found", icon = "error")
#     return(invisible(NULL))
#   }

#   Selected <- svalue(.rqda$.CodeCatWidget)

#   if (identical(Selected, character(0))) {
#     gmessage("Select a code category first", icon = "warning")
#     return(invisible(NULL))
#   }

#   if (!gconfirm("Really delete this code category?", icon = "question")) {
#     return(invisible(NULL))
#   }

#   catid <- rqda_sel(sprintf(
#     "SELECT catid FROM codecat WHERE status = 1 AND name = '%s'",
#     gsub("'", "''", Selected)
#   ))

#   if (!is.null(catid) && nrow(catid) > 0) {
#     catid <- catid[1, 1]

#     # Delete category
#     rqda_exe(sprintf(
#       "UPDATE codecat SET status = 0 WHERE name = '%s'",
#       gsub("'", "''", Selected)
#     ))

#     # Delete relationships
#     rqda_exe(sprintf(
#       "UPDATE treecode SET status = 0 WHERE catid = %d",
#       catid
#     ))

#     UpdateTableWidget(
#       Widget = .rqda$.CodeCatWidget,
#       FromdbTable = "codecat"
#     )

#     UpdateCodeofCatWidget()

#     message("Deleted code category: ", Selected)
#   }
# }

# #' Update code-of-category widget
# #' @export
# UpdateCodeofCatWidget <- function() {
#   if (!is_projOpen()) return(invisible(NULL))

#   if (!exists(".CodeCatWidget", envir = .rqda) || !exists(".CodeofCat", envir = .rqda)) {
#     return(invisible(NULL))
#   }

#   Selected <- svalue(.rqda$.CodeCatWidget)

#   if (identical(Selected, character(0))) {
#     tryCatch(.rqda$.CodeofCat[] <- NULL, error = function(e) {})
#     return(invisible(NULL))
#   }

#   catid <- rqda_sel(sprintf(
#     "SELECT catid FROM codecat WHERE status = 1 AND name = '%s'",
#     gsub("'", "''", Selected)
#   ))

#   if (is.null(catid) || nrow(catid) == 0) {
#     tryCatch(.rqda$.CodeofCat[] <- NULL, error = function(e) {})
#     return(invisible(NULL))
#   }

#   catid <- catid[1, 1]

#   # Get codes in this category
#   codes <- rqda_sel(sprintf(
#     paste(
#       "SELECT f.name FROM treecode t ",
#       "LEFT JOIN freecode f ON t.cid = f.id ",
#       "WHERE t.catid = %d AND t.status = 1 AND f.status = 1"
#     ),
#     catid
#   ))

#   if (!is.null(codes) && nrow(codes) > 0) {
#     Encoding(codes$name) <- "UTF-8"
#     tryCatch(.rqda$.CodeofCat[] <- codes$name, error = function(e) {})
#   } else {
#     tryCatch(.rqda$.CodeofCat[] <- NULL, error = function(e) {})
#   }

#   invisible(NULL)
# }

# #' Add File Category
# #' @export
# AddFileCat <- function(catname = NULL) {
#   if (!is_projOpen()) return(invisible(NULL))

#   if (is.null(catname)) {
#     catname <- ginput(
#       message = "Enter new File Category:",
#       text = "",
#       title = "New File Category"
#     )
#   }

#   if (identical(catname, character(0)) || catname == "") {
#     return(invisible(NULL))
#   }

#   Encoding(catname) <- "UTF-8"
#   AddTodbTable(catname, "filecat", Id = "catid")

#   if (exists(".FileCatWidget", envir = .rqda)) {
#     UpdateTableWidget(
#       Widget = .rqda$.FileCatWidget,
#       FromdbTable = "filecat"
#     )
#   }

#   message("Added file category: ", catname)
# }

# #' Delete File Category
# #' @export
# DeleteFileCat <- function() {
#   if (!is_projOpen()) return(invisible(NULL))

#   if (!exists(".FileCatWidget", envir = .rqda)) {
#     gmessage("File category widget not found", icon = "error")
#     return(invisible(NULL))
#   }

#   Selected <- svalue(.rqda$.FileCatWidget)

#   if (identical(Selected, character(0))) {
#     gmessage("Select a file category first", icon = "warning")
#     return(invisible(NULL))
#   }

#   if (!gconfirm("Really delete this file category?", icon = "question")) {
#     return(invisible(NULL))
#   }

#   catid <- rqda_sel(sprintf(
#     "SELECT catid FROM filecat WHERE status = 1 AND name = '%s'",
#     gsub("'", "''", Selected)
#   ))

#   if (!is.null(catid) && nrow(catid) > 0) {
#     catid <- catid[1, 1]

#     # Delete category
#     rqda_exe(sprintf(
#       "UPDATE filecat SET status = 0 WHERE name = '%s'",
#       gsub("'", "''", Selected)
#     ))

#     # Delete relationships
#     rqda_exe(sprintf(
#       "UPDATE treefile SET status = 0 WHERE catid = %d",
#       catid
#     ))

#     UpdateTableWidget(
#       Widget = .rqda$.FileCatWidget,
#       FromdbTable = "filecat"
#     )

#     UpdateFileofCatWidget()

#     message("Deleted file category: ", Selected)
#   }
# }

# #' Update file-of-category widget
# #' @export
# UpdateFileofCatWidget <- function() {
#   if (!is_projOpen()) return(invisible(NULL))

#   if (!exists(".FileCatWidget", envir = .rqda) || !exists(".FileofCat", envir = .rqda)) {
#     return(invisible(NULL))
#   }

#   Selected <- svalue(.rqda$.FileCatWidget)

#   if (identical(Selected, character(0))) {
#     tryCatch(.rqda$.FileofCat[] <- NULL, error = function(e) {})
#     return(invisible(NULL))
#   }

#   catid <- rqda_sel(sprintf(
#     "SELECT catid FROM filecat WHERE status = 1 AND name = '%s'",
#     gsub("'", "''", Selected)
#   ))

#   if (is.null(catid) || nrow(catid) == 0) {
#     tryCatch(.rqda$.FileofCat[] <- NULL, error = function(e) {})
#     return(invisible(NULL))
#   }

#   catid <- catid[1, 1]

#   # Get files in this category
#   files <- rqda_sel(sprintf(
#     paste(
#       "SELECT s.name FROM treefile t ",
#       "LEFT JOIN source s ON t.fid = s.id ",
#       "WHERE t.catid = %d AND t.status = 1 AND s.status = 1"
#     ),
#     catid
#   ))

#   if (!is.null(files) && nrow(files) > 0) {
#     Encoding(files$name) <- "UTF-8"
#     tryCatch(.rqda$.FileofCat[] <- files$name, error = function(e) {})
#   } else {
#     tryCatch(.rqda$.FileofCat[] <- NULL, error = function(e) {})
#   }

#   invisible(NULL)
# }
