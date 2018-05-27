library(shiny)
library(sigmajs)
library(shinythemes)

ui <- navbarPage(
    "sigmajs",
    theme = shinytheme("paper"),
    tabPanel(
        "home",
        fluidRow(
            column(1),
            column(7, h1("sigmajs*"), br(), h4("for shiny*")),
            column(3, img(src = "https://raw.githubusercontent.com/JohnCoene/sigmajs/master/man/figures/logo.png?token=AHeIxJYJW3eqBk4LRBFIPmv--xy0YZr2ks5bFBOwwA%3D%3D",
                class = "img-responsive"
                )
            ),
            column(1)
        )
    ),
    tabPanel(
        "forceAtlas2",
        fluidRow(
            column(1, actionButton("start", "Start layout")),
            column(1, actionButton("stop", "Stop layout"))
        ),
        fluidRow(
            column(12, sigmajsOutput("forceAtlas2"))
        )
    )
)

server <- function(input, output){

    nodes <- sg_make_nodes(50)
    edges <- sg_make_edges(nodes, 75)
    
    output$forceAtlas2 <- renderSigmajs({
        sigmajs() %>%
            sg_nodes(nodes, id, size, color) %>%
            sg_edges(edges, id, source, target)
    })

	observeEvent(input$start, {
		sigmajsProxy("forceAtlas2") %>%
				sg_force_start_p(worker = TRUE)
	})

	observeEvent(input$stop, {
		sigmajsProxy("forceAtlas2") %>%
				sg_force_stop_p()
	})
}

shinyApp(ui, server)