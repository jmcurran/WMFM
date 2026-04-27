# Refactoring Context: What is my fitted model App

## Overview

The "What is my fitted model App" is a complex R Shiny application with a very large `server()` function (approximately 4,000 lines). This size indicates that multiple responsibilities are currently tightly coupled, making the code difficult to:

- Understand
- Maintain
- Test
- Extend safely

This document defines the context and guiding principles for refactoring the app into a modular, maintainable architecture.

---

## Current Problem

The current `server()` function:

- Contains multiple responsibilities (data processing, modelling, UI reactions, plotting, exporting, etc.)
- Mixes pure logic with Shiny reactivity
- Is difficult to navigate and reason about
- Increases risk when making changes

This is a classic **monolithic server anti-pattern** in Shiny.

---

## Refactoring Goals

The refactor aims to:

1. **Reduce cognitive load**
   - Smaller, focused components
   - Clear separation of concerns

2. **Improve maintainability**
   - Easier to modify individual features
   - Localised changes with reduced side effects

3. **Enable reuse**
   - Extract reusable modelling and data logic

4. **Improve testability**
   - Move logic into pure functions where possible

5. **Stabilise the app structure**
   - Make the server function an orchestration layer

---

## Target Architecture

### 1. Thin Server Function

The top-level `server()` should become a coordinator:

```r
server = function(input, output, session) {
  appData = dataServer("data")

  filtersServer("filters", appData)
  modelServer("model", appData)
  diagnosticsServer("diagnostics", appData)
  resultsServer("results", appData)
  exportServer("export", appData)
}
```

Target size: **~100–200 lines**

---

### 2. Shiny Modules by Feature

Each major feature should be implemented as a module:

- Data loading / preparation
- Model fitting
- Diagnostics / plots
- Results summaries
- Export / reporting

Example:

```r
modelServer = function(id, dataReactive) {
  moduleServer(id, function(input, output, session) {
    fittedModel = reactive({
      fitModel(dataReactive(), input$modelType)
    })

    return(fittedModel)
  })
}
```

---

### 3. Pure Functions for Core Logic

All non-reactive logic should be extracted into standalone functions:

```r
fitModel = function(data, modelType) {
  ...
}
```

These should:

- Not depend on `input`, `output`, or `session`
- Be reusable outside Shiny
- Be testable with `testthat`

---

### 4. File Structure

Organise the project into clear functional areas:

```text
R/
  app-server.R
  app-ui.R

  mod-data.R
  mod-filters.R
  mod-model.R
  mod-diagnostics.R
  mod-results.R
  mod-export.R

  data-processing.R
  modelling.R
  plotting.R
  reporting.R
```

---

### 5. Reactive Design Principles

- Prefer **small, explicit reactives**
- Pass reactives into modules rather than using global state
- Avoid large shared `reactiveValues()` objects unless necessary
- Keep reactive chains shallow and understandable

---

### 6. Incremental Refactoring Strategy

Do **not** rewrite everything at once.

Recommended approach:

1. Identify a self-contained feature (e.g. plotting or model fitting)
2. Extract pure helper functions
3. Wrap the feature into a module
4. Replace the corresponding block in `server()`
5. Test the app
6. Repeat

---

## Design Principles

- **Single responsibility per module**
- **Explicit data flow**
- **Minimal hidden state**
- **Composable functions**
- **Readable over clever**

---

## Expected Outcome

After refactoring:

- The app will be easier to navigate and extend
- Bugs will be easier to isolate
- New features can be added without touching unrelated code
- Core modelling logic will be reusable outside the app

---

## Notes for This Project

- Preserve existing behaviour during refactoring
- Avoid breaking user-facing functionality
- Refactor in small, verifiable steps
- Maintain consistent R coding style (camelCase, `=`, braces)

---

## Summary

The goal is not just to reduce line count, but to **transform the architecture**:

> From a monolithic, tightly coupled server  
> → to a modular, composable, and testable system

This will significantly improve long-term sustainability of the "What is my fitted model App".
