# Stage 48.5.1: toolbar CSS parse repair

Stage 48.5 introduced the compact statistical insertion toolbar and modal
selection workflow. Package documentation could not be generated because the
CSS embedded in `R/app-ui.R` used unescaped double quotation marks around the
`Times New Roman` font name inside an R double-quoted string.

Stage 48.5.1 changes the CSS font-family declaration to use single quotation
marks around the multi-word font name. This preserves the intended browser
styling while allowing `R/app-ui.R` to parse correctly.

No statistical calculations, modal behaviour, editor behaviour, or formative
feedback logic are changed.
