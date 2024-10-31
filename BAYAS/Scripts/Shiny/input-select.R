
# Create tags for each of the options; use <optgroup> if necessary.
# This returns a HTML string instead of tags for performance reasons.
selectOptions <- function(choices, selected = NULL, inputId, perfWarning = FALSE) {
  if (length(choices) >= 1000) {
    warning("The select input \"", inputId, "\" contains a large number of ",
            "options; consider using server-side selectize for massively improved ",
            "performance. See the Details section of the ?selectizeInput help topic.",
            call. = FALSE)
  }
  
  html <- mapply(choices, names(choices), FUN = function(choice, label) {
    if (is.list(choice)) {
      # If sub-list, create an optgroup and recurse into the sublist
      sprintf(
        '<optgroup label="%s">\n%s\n</optgroup>',
        htmlEscape(label, TRUE),
        selectOptions(choice, selected, inputId, perfWarning)
      )
      
    } else {
      # If single item, just return option string
      sprintf(
        '<option value="%s"%s>%s</option>',
        htmlEscape(choice, TRUE),
        if (choice %in% selected) ' selected' else '',
        htmlEscape(label)
      )
    }
  })
  
  HTML(paste(html, collapse = '\n'))
}
