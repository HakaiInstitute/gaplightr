# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Workflow Requirements

- Review and update CLAUDE.md regularly as the codebase evolves
- Always ask clarifying questions in planning mode

## Key development commands

General advice: - When running R from the console, always run it with
`--quiet --vanilla` - Always run `air format .` after generating code

## Project Overview

**gaplightr** is an R package currently in early development (version
0.0.0.9000).

## Package Structure

Standard R package layout: - `R/` - Package functions - `man/` -
Documentation (auto-generated via roxygen2) - `DESCRIPTION` - Package
metadata and dependencies - `NAMESPACE` - Package namespace
(auto-generated via roxygen2)

## Development Commands

### Documentation

``` r
# Generate documentation from roxygen2 comments
devtools::document()
```

### Building and Checking

``` r
# Load package for interactive development
devtools::load_all()

# Run R CMD check
devtools::check()

# Install package locally
devtools::install()
```

### Testing

``` r
# Run all tests (when tests/ exists)
devtools::test()

# Run tests with coverage
covr::package_coverage()
```

## Documentation Standards

- Use roxygen2 for function documentation with markdown enabled
  (`Roxygen: list(markdown = TRUE)`)
- All exported functions must have `@export` tag
- Include `@param`, `@return`, and `@examples` sections
- Run
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  after modifying roxygen comments

## Adding Dependencies

- Use `usethis::use_package("package_name")` to add dependencies to
  DESCRIPTION
- Use `usethis::use_package("package_name", "Suggests")` for suggested
  packages
- Reference external functions with `::` (e.g., `dplyr::filter()`) or
  import in NAMESPACE
