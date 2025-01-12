# function used in shiny
#' @import esquisse
palette_ui <- function(id) {
  ns <- NS(id)
  pals <- esquisse:::get_palettes()
  tagList(
    tags$style(
      ".bootstrap-select .dropdown-menu li a span.text { display: block !important; }"
    ),
    radioButtons(
      inputId = ns("type"),
      label = NULL,
      choiceValues = c("palette", "manual"),
      choiceNames = tagList(
        tags$div(
          tags$b("使用色板"),
          palettePicker(
            inputId = ns("palette"),
            label = NULL,
            choices = pals$choices,
            textColor = pals$textColor,
            pickerOpts = list(container = "body")
          ),
          checkboxInput(
            inputId = ns("reverse"),
            value = FALSE,
            label = "Reverse colors?"
          )
        ),
        tags$div(
          tags$b("使用特定颜色"),
          uiOutput(outputId = ns("manual"))
        )
      )
    )
  )
}

palette_server <- function(id, all_samples) {
  ## palettes names
  palettes <- esquisse:::get_palettes()
  palettes <- palettes$choices
  palettes <- unlist(palettes, recursive = FALSE, use.names = TRUE)
  names(palettes) <- gsub("^.+\\.", "", names(palettes))
  
  callModule(
    id = id,
    function(input, output, session) {
      ns <- session$ns
      ## 
      colors_manual <- reactiveValues(x = NULL, type = 'discrete')
      output$manual <- renderUI({
        req(all_samples)
        ## get colors
        unique_sample <- sort(unique(all_samples))
        colors <- colorRampPalette(palettes[[input$palette]])(length(unique_sample))
        colors_id <- paste0("colors_", esquisse:::makeId(unique_sample))
        ## set muanual color
        colors_manual$x <- setNames(as.list(colors_id), unique_sample)
        # colors_manual$type <- "discrete"
        lapply(
          X = seq_along(unique_sample),
          FUN = function(i) {
            tagList(
              tags$span(
                tagAppendAttributes(
                  colorPickr(
                    inputId = ns(colors_id[i]),
                    selected = colors[i],
                    label = NULL,
                    theme = "classic",
                    useAsButton = TRUE,
                    update = "save",
                    interaction = list(
                      hex = FALSE,
                      rgba = FALSE,
                      input = TRUE,
                      save = TRUE,
                      clear = FALSE
                    )
                  ),
                  style = "display: inline; vertical-align: middle;"
                ),
                unique_sample[i]
              ),
              tags$br()
            )
          }
        )
      })
      ## update colors
      observeEvent(colors_manual$type, {
         pals <- esquisse:::get_palettes()
         esquisse:::updatePalettePicker(
           inputId = "palette",
           choices = pals$choices,
           textColor = pals$textColor, 
           selected = isolate(input$palette)
         )
      }, ignoreInit = TRUE)
      
      ## return result
      return(
        reactive({
          if (identical(input$type, "palette")) {
            list(
              scale = "palette",
              reverse = input$reverse,
              colors = input$palette
            )
          } else {
            ids <- colors_manual$x
            list(
              scale = "manual",
              type = colors_manual$type,
              colors = lapply(
                X = ids,
                FUN = function(x) {
                  input[[x]]
                }
              )
            )
          }
        })
      )
      
    }
  )
}
