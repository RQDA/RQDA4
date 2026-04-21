# Case-related buttons for RQDA

AddCaseButton <- function(label = gettext("Add", domain = "R-RQDA")) {
  btn <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        AddCase()
      }, error = function(e) {
        message("Error adding case: ", e$message)
      })
    }
  )

  if (!exists("button", envir = .GlobalEnv)) {
    button_env <- new.env(parent = emptyenv())
    assign("button", button_env, envir = .GlobalEnv)
  } else {
    button_env <- get("button", envir = .GlobalEnv)
  }
  assign("AddCasB", btn, envir = button_env)

  enabled(btn) <- FALSE
  btn
}

DeleteCaseButton <- function(label = gettext("Delete", domain = "R-RQDA")) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        DeleteCase()
      }, error = function(e) {
        message("Error deleting case: ", e$message)
      })
    }
  )

  button
}

Case_RenameButton <- function(label, ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        RenameCase()
      }, error = function(e) {
        message("Error renaming case: ", e$message)
      })
    }
  )

  button
}

Case_MemoButton <- function(label = gettext("Memo", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        CaseMemo()
      }, error = function(e) {
        message("Error opening case memo: ", e$message)
      })
    }
  )

  button
}

Case_AddFileButton <- function(label = gettext("Add File", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        AddFileToCase()
      }, error = function(e) {
        message("Error adding file to case: ", e$message)
      })
    }
  )

  button
}

Case_FileButton <- function(label = gettext("File Of Case", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      tryCatch({
        UpdateFileofCaseWidget()
      }, error = function(e) {
        message("Error updating file-of-case: ", e$message)
      })
    }
  )

  button
}

Case_UnlinkButton <- function(label = gettext("Unlink", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      # Unlink selected file from selected case
      casename <- tryCatch(.rqda$selected_case, error=function(e) NULL)
      if (is.null(casename) || !nzchar(casename)) {
        gmessage("Select a case first.", icon="warning"); return()
      }
      fname <- tryCatch({
        sel <- as.character(svalue(.rqda$.FileofCase))
        sel <- sel[nzchar(sel)]
        if (length(sel)>0) sel[1] else NULL
      }, error=function(e) NULL)
      if (is.null(fname)) {
        gmessage("Select a file in the case first.", icon="warning"); return()
      }
      cid <- rqda_sel(sprintf("SELECT id FROM cases WHERE name='%s' AND status=1",
                              gsub("'","''",casename)))
      fid <- rqda_sel(sprintf("SELECT id FROM source WHERE name='%s' AND status=1",
                              gsub("'","''",fname)))
      if (is.null(cid)||nrow(cid)==0||is.null(fid)||nrow(fid)==0) return()
      rqda_exe(sprintf("UPDATE caselinkage SET status=0 WHERE caseid=%d AND fid=%d",
                       cid$id[1], fid$id[1]))
      message("Unlinked '", fname, "' from case '", casename, "'")
      tryCatch(UpdateFileofCaseWidget(), error=function(e){})
    }
  )

  button
}

CaseAttribute_Button <- function(label = gettext("Attributes", domain = "R-RQDA"), ...) {
  button <- gbutton(
    label,
    handler = function(h, ...) {
      casename <- tryCatch(.rqda$selected_case, error=function(e) NULL)
      if (is.null(casename) || !nzchar(casename)) {
        gmessage("Select a case first.", icon="warning"); return()
      }
      tryCatch(.show_attr_editor("case", casename), error=function(e) message(e$message))
    }
  )

  button
}
