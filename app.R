library(DT)
library(shiny)
library(sigmajs)
library(crosstalk)
library(shinythemes)

ui <- navbarPage(
    "sigmajs",
	inverse = FALSE,
    theme = shinytheme("flatly"),
    tabPanel(
        "home",
        fluidRow(
            column(1),
            column(7, 
				fluidRow(
					column(12, h1("sigmajs*"), br(), h4("*for shiny"), br())
				),
				fluidRow(
					column(1, 
						img(
							src = "http://twinetverse.john-coene.com/reference/figures/logo.png",
							class = "img-responsive"
						)
					),
					column(5,
						p("Part of the", a("twinetverse,", href = "http://twinetverse.john-coene.com/", target = "_blank"), "a universe of packages for Twitter network analysis and visualisations.")
					),
					column(6)
				)
			),
            column(3, 
				img(
					src = "http://sigmajs.john-coene.com/reference/figures/logo.png",
                	class = "img-responsive"
                )
            ),
            column(1)
        ),
		fluidRow(
			column(1),
			column(1, h4(tags$a("Docs", href = "http://sigmajs.john-coene.com/", target = "_blank", class = "btn btn-primary"))),
			column(2, h4(tags$a("App source code", href = "https://github.com/JohnCoene/sigmajshiny", target = "_blank", class = "btn btn-default"))),
			column(8)
		)
    ),
    tabPanel(
        "forceAtlas2",
        fluidRow(
            column(1, actionButton("start", "Start layout", class = "btn btn-success", icon = icon("play"))),
			column(1, p("")),
            column(1, actionButton("stop", "Stop layout", class = "btn btn-danger", icon = icon("stop"))),
			column(9)
        ),
        fluidRow(
            column(12, sigmajsOutput("forceAtlas2", height = "97vh"))
        )
    ),
    tabPanel(
        "Add",
        fluidRow(
            column(2, actionButton("add", "Add node & edge", , class = "btn btn-primary", icon = icon("plus"))),
            column(2, actionButton("start2", "Start force", class = "btn btn-success", icon = icon("play"))),
            column(2, actionButton("stop2", "Kill force", class = "btn btn-danger", icon = icon("heartbeat"))),
			column(6)
        ),
        fluidRow(
            column(12, sigmajsOutput("addNodesEdges", height = "97vh"))
        )
    ),
	tabPanel(
		"Drop",
		fluidRow(
			column(2, actionButton("dropNode", "Drop a node", class = "btn btn-danger", icon = icon("times"))),
			column(2, actionButton("dropEdge", "Drop an edge", class = "btn btn-danger", icon = icon("times-circle-o")))
		),
		fluidRow(
			sigmajsOutput("dropNodesEdges", height = "97vh")
		)
	),
    tabPanel(
        "Delay",
        fluidRow(
            column(3, actionButton("add3", "Add nodes & edges", , class = "btn btn-success", icon = icon("plus")))
        ),
        sigmajsOutput("sg", height = "97vh")
    ),
    tabPanel(
        "Filter",
        fluidRow(
            column(3, sliderInput("slider", "Filter nodes", value = 0, min = 0, max = 200))
        ),
        sigmajsOutput("filter", height = "97vh")
    ),
	tabPanel(
		"Crosstalk",
		fluidRow(
			column(
				DTOutput("dtNodes"),
				width = 4
			),
			column(
				width = 8,
				sigmajsOutput("sgCrosstalk")
			)
		)
	),
	tabPanel(
		"Events",
		h4("Interact with the graph"),
		fluidRow(
			column(9, sigmajsOutput("sgEvents")),
			column(3, p("There are plenty more events to capture, all official events are available:"),
				a(href = "https://github.com/jacomyal/sigma.js/wiki/Events-API", "official documentation"),
				h4("Background right clicked"), verbatimTextOutput("bgClicked")
			)
		),
		fluidRow(
			column(3, h4("Node clicked"), verbatimTextOutput("nodeClicked")),
			column(3, h4("Node hovered"), verbatimTextOutput("nodeHovered"))
		)
	)
)

server <- function(input, output, session){

    nodes <- sg_make_nodes(50)
    edges <- sg_make_edges(nodes, 75)
    
    output$forceAtlas2 <- renderSigmajs({
        sigmajs() %>%
            sg_nodes(nodes, id, size, color) %>%
            sg_edges(edges, id, source, target)
    })

    output$addNodesEdges <- renderSigmajs({
        sigmajs() %>%
            sg_nodes(nodes, id, size, color) %>%
            sg_edges(edges, id, source, target)
    })

	# initialise "empty" visualisation
	output$sg <- renderSigmajs({
		sigmajs(type = "webgl") %>% # use webgl
			sg_force()
	})

    output$sgEvents <- renderSigmajs({
        sigmajs() %>%
            sg_nodes(nodes, id, label, size, color) %>%
            sg_edges(edges, id, source, target) %>%
			sg_layout()
    })

	filter_nodes <- sg_make_nodes(100)
	filter_edges <- sg_make_edges(nodes)
	filter_nodes$size <- runif(100, 1, 200)

	output$filter <- renderSigmajs({
        sigmajs() %>%
            sg_nodes(filter_nodes, id, size, color) %>%
            sg_edges(filter_edges, id, source, target) %>%
			sg_layout()
	})

	observeEvent(input$start, {
		sigmajsProxy("forceAtlas2") %>%
				sg_force_start_p(worker = TRUE)
	})

	observeEvent(input$stop, {
		sigmajsProxy("forceAtlas2") %>%
				sg_force_kill_p()
	})

	i <- nrow(edges)
	j <- nrow(nodes)

	observeEvent(input$add, {

		i <<- i + 1
		j <<- j + 1

		edges_add <- data.frame(
			id = i,
			source = sample(1:j, 1),
			target = sample(1:j, 1)
		)

		nodes_add <- data.frame(
			id = i,
			size = runif(1, 1, 5),
            color = "#B1E2A3",
			label = sample(LETTERS, 1)
		)

		sigmajsProxy("addNodesEdges") %>%
			sg_add_edge_p(edges_add, id, source, target) %>%
			sg_add_node_p(nodes_add, id, label, size, color)
	})

	observeEvent(input$start2, {
		sigmajsProxy("addNodesEdges") %>%
				sg_force_start_p(worker = TRUE)
	})

	observeEvent(input$stop2, {
		sigmajsProxy("addNodesEdges") %>%
				sg_force_kill_p()
	})

	ids <- as.character(1:100) # create 100 nodes
	n <- 150 # number of edges

	# create edges with random delay FIRST
	edges2 <- data.frame(
		id = 1:n,
		source = sample(ids, n, replace = TRUE),
		target = sample(ids, n, replace = TRUE),
		created_at = cumsum(ceiling(rnorm(n, 500, 50))),
		stringsAsFactors = FALSE
	)

	# get source and target
	src <- dplyr::select(edges2, id = source, created_at)
	tgt <- dplyr::select(edges2, id = target, created_at)

	# nodes appear at their first edge appearance
	nodes2 <- src %>%
		dplyr::bind_rows(tgt) %>% # bind edges source/target to have "nodes"
		dplyr::group_by(id) %>% # find minimum by id - when node should appear
		dplyr::summarise(
			appear_at = min(created_at) - 1 # Minus one millisecond to ensure node appears BEFORE any edge connecting to it
		) %>%
		dplyr::ungroup() %>%
		dplyr::mutate( # add labels, color and size
			label = sample(LETTERS, n(), replace = TRUE), 
			size = runif(n(), 1, 5),
			color = colorRampPalette(c("#B1E2A3", "#98D3A5", "#328983", "#1C5C70", "#24C96B"))(n())
		)

	# add nodes and edges with delay
	observeEvent(input$add3, {
		sigmajsProxy("sg") %>%
			sg_add_nodes_delay_p(nodes2, appear_at, id, label, size, color, cumsum = FALSE, refresh = TRUE) %>%
			sg_add_edges_delay_p(edges2, created_at, id, source, target, cumsum = FALSE, refresh = TRUE)
	})

    output$nodeClicked <- renderPrint({
        input$sgEvents_click_node
    })

    output$nodeHovered <- renderPrint({
        input$sgEvents_over_node
    })

    output$bgClicked <- renderPrint({
        input$sgEvents_right_click_stage
    })

	output$dropNodesEdges <- renderSigmajs({
        sigmajs() %>%
            sg_nodes(nodes, id, size, color) %>%
            sg_edges(edges, id, source, target) %>%
			sg_layout()
	})

	dropped_nodes <- NULL

	observeEvent(input$dropNode, {

		id <- sample(nodes$id[!nodes$id %in% dropped_nodes], 1)

		dropped_nodes <<- id

		sigmajsProxy("dropNodesEdges") %>%
			sg_drop_node_p(id)
	})

	dropped_edges <- NULL

	observeEvent(input$dropEdge, {

		id <- sample(edges$id[!edges$id %in% dropped_edges], 1)

		dropped_edges <<- id

		sigmajsProxy("dropNodesEdges") %>%
			sg_drop_edge_p(id)
	})

	observeEvent(input$slider, {
		sigmajsProxy("filter") %>%
			sg_filter_gt_p(input$slider, "size")
	})

	sd <- SharedData$new(nodes, key = nodes$id)

	output$dtNodes <- renderDT({
		DT::datatable(sd)
	}, server = FALSE)

	output$sgCrosstalk <- renderSigmajs({
		sigmajs() %>%
		  sg_nodes(sd, id, label, size, color) %>%
		  sg_edges(edges, id, source, target) %>%
		  sg_layout()
	})

}

shinyApp(ui, server)