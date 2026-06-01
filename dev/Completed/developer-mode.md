# Developer mode

Developer mode is a maintainer/debugging pathway and is hidden from ordinary users.

To show the developer-mode toggle in a local development environment, add this to `~/.Renviron` and restart R:

```text
WMFM_SHOW_DEVELOPER_MODE=1
```

Developer mode is session-only and is not saved in the WMFM local config file. Provider settings are independent of developer mode.
