# RQDA4

> [!WARNING]
> **Pre-alpha software.** This is a GTK4 port built primarily to exercise the [Rgtk4](https://github.com/JanMarvin/Rgtk4) bindings. It is not ready for research use. Data loss is possible, especially when importing or editing files. Always keep backups of your projects.

`RQDA` is a rewrite of the [`RQDA`](https://rqda.r-forge.r-project.org/) qualitative data analysis package for R, updated to run on GTK4 via [`Rgtk4`](https://github.com/JanMarvin/Rgtk4) bindings. The original RQDA was built on `RGtk2`, which no longer runs on current desktop environments.

## What it does

RQDA is a tool for qualitative coding — reading through text documents and marking passages with codes (themes, categories, labels). The basic workflow:

- Create a project (stored as a SQLite database)
- Import plain-text documents
- Define codes and organise them into categories
- Mark passages in documents with one or more codes
- Organise files into cases and assign attributes (metadata such as demographics)
- Retrieve and browse coded passages, cross-reference cases and codes, export results

The database format is compatible with the original RQDA 0.2.x — existing projects can be opened. New columns added by RQDA4 (`freecode.color`, `attributes.class`) are added via `ALTER TABLE ADD COLUMN` with `tryCatch` and are silently ignored if you open the project in the legacy version again.

Non-GUI functions (`rqda_sel`, `rqda_exe`, `summaryCodings`, `queryFiles`, `filesByCodes`, `getCodingTable`, `%and%`, `%or%`, `%not%`, etc.) work in an R console session without a GUI.

## Installation

Install GTK4 for your platform first (see [System requirements](#system-requirements)), then:

```r
# Install Rgtk4 from GitHub
remotes::install_github("JanMarvin/Rgtk4")

# Install RQDA4
remotes::install_github("RQDA/RQDA4")
```

Launch:

```r
library(RQDA)
RQDA()
```

## Status

This is a showcase and test bed for `Rgtk4`. Most of the original `RQDA` functionality is present but things are rough around the edges. If you find a bug or know GTK4 well, contributions are welcome.

## Original RQDA

Written by Ronggui Huang.  
Project page: <https://rqda.r-forge.r-project.org/>  
Source: <https://github.com/RQDA/RQDA>

## License

BSD 3-Clause — same as the original RQDA.

## Development note

This port was created using vibe coding with [Claude](https://claude.ai) (Anthropic). The human author provided direction, tested the output, and reported bugs; Claude generated the majority of the R code. Porting ~20 R source files from RGtk2 to GTK4 in a few days would not have been practical any other way — the GTK4 API surface is large, the Rgtk4 bindings are underdocumented, and the original RQDA codebase predates modern R conventions throughout.

If something looks odd, it probably is.
