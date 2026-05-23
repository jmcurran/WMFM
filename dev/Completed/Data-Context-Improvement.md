Future UX refinement: improve data-context status feedback

Current behavior:
The app shows a separate text message ("Data context has been provided") below the response-variable controls after data context is entered.

Future improvement idea:
- Move the status message so it appears directly underneath the data-context button.
- Make the button visually stateful:
  - warning/red styling when no data context has been provided
  - success/green styling when data context exists
- Update button text dynamically:
  - "Provide data context" when missing
  - "Edit data context" or "Data context provided" when present
- Keep this as a UI-only refinement with no changes to the underlying data-context workflow.
- Preserve current functionality and avoid layout churn.
