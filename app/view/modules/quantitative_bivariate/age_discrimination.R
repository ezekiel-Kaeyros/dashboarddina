
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config,style,ggplotly],
  magrittr[`%>%`],
  ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative_bivariate_page_data,
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Häufigkeit anderer Formen von Diskriminierung nach Altersgruppe",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    # Graph goes here
                    uiOutput(ns("plot_personage"))
                )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    #output$plot_personaf <- render
    button_state <- reactiveVal(FALSE)
    yiord_palette <- c("#e6eff0", "#cddee1", "#b3ced3", "#9abdc4", "#81adb5", "#689ca6", "#4f8c97", "#357b89",
                       "#1c6b7a", "#035a6b", "#035160", "#024856", "#023f4b", "#023640", "#022d36", "#01242b",
                       "#011b20")

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "table"))
      }
    })

    toggle <- reactiveValues(piechart = TRUE)
    output$plot_personage <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })

    output$barplot <- renderPlotly({
      plotly::plot_ly(quantitative_bivariate_page_data$table_age_disc, x = ~category_age, y = ~Freq, color = ~discrimination, type = "bar", colors = yiord_palette,
                      text = ~paste("Altersgruppe: ", category_age, "<br>Frequenz: ", Freq, "<br>Diskriminierung: ", discrimination)) %>%
        layout(#title = "Frequency of Different Forms of Discrimination by Age Group",
               xaxis = list(title = "Altersgruppe"),
               yaxis = list(title = "Frequenz"),
               barmode = "group")%>%
        style(hoverinfo = "text")
    })

    output$piechart <- renderPlotly({
      gg<-ggplot(quantitative_bivariate_page_data$table_age_disc, aes(category_age, discrimination)) +
        geom_tile(aes(fill = Freq)) +
        geom_text(aes(label = round(Freq, 1), text = paste("Altersgruppe:", category_age, "\nDiskriminierung:", discrimination, "\nZählen Sie:", Freq))) +
        scale_fill_gradient(low = "#e6eff0", high = "#011b20") +
        labs(#title = "Frequency of Different Forms of Discrimination by Age Group",
             x = "Altersgruppe",
             y = "Diskriminierung",
             fill = "Zählen Sie") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(gg, tooltip = "text")
    })

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })


  })
}

