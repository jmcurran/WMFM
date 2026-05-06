# Stage 17.12.2 server inventory

Follow-up test repair for the Stage 17.12 structural startup extraction.

## Change

- Updated the developer-mode source-control test so the password-verification assertion also searches the extracted developer-mode helper file.
- Kept the structural extraction unchanged.

## Rationale

The application behaviour is unchanged, but the test expectation needed to follow the new source-file boundary introduced during the app-server refactor.
