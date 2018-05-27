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
            column(12, sigmajsOutput("forceAtlas2", height = "97vh"))
        )
    ),
    tabPanel(
        "Add",
        fluidRow(
            column(4, actionButton("add", "Add node & edge")),
            column(4, actionButton("start2", "Start force")),
            column(4, actionButton("stop2", "Stop force"))
        ),
        fluidRow(
            column(12, sigmajsOutput("addNodesEdges", height = "97vh"))
        )
    ),
    tabPanel(
        "Delay",
        fluidRow(
            column(3, actionButton("add3", "add nodes & edges"))
        ),
        sigmajsOutput("sg", height = "97vh")
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

    output$addNodesEdges <- renderSigmajs({
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

	i <- nrow(edges)
	j <- nrow(nodes)

	observeEvent(input$add, {

		i <<- i + 1
		j <<- j + 1

		edges <- data.frame(
			id = i,
			source = sample(1:j, 1),
			target = sample(1:j, 1)
		)

		nodes <- data.frame(
			id = i,
			size = runif(1, 1, 5),
            color = "#B1E2A3",
			label = sample(LETTERS, 1)
		)

		sigmajsProxy("addNodesEdges") %>%
			sg_add_edge_p(edges, id, source, target) %>%
			sg_add_node_p(nodes, id, label, size, color)
	})

	observeEvent(input$start2, {
		sigmajsProxy("addNodesEdges") %>%
				sg_force_start_p(worker = TRUE)
	})

	observeEvent(input$stop2, {
		sigmajsProxy("addNodesEdges") %>%
				sg_force_stop_p()
	})

ids <- as.character(1:100) # create 100 nodes
	n <- 150 # number of edges

	# create edges with random delay FIRST
	edges <- data.frame(
		id = 1:n,
		source = sample(ids, n, replace = TRUE),
		target = sample(ids, n, replace = TRUE),
		created_at = cumsum(ceiling(rnorm(n, 500, 50))),
		stringsAsFactors = FALSE
	)

	# get source and target
	src <- dplyr::select(edges, id = source, created_at)
	tgt <- dplyr::select(edges, id = target, created_at)

	# nodes appear at their first edge appearance
	nodes <- src %>%
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

	# initialise "empty" visualisation
	output$sg <- renderSigmajs({
		sigmajs(type = "webgl") %>% # use webgl
			sg_force()
	})

	# add nodes and edges with delay
	observeEvent(input$add3, {
		sigmajsProxy("sg") %>%
			sg_add_nodes_delay_p(nodes, appear_at, id, label, size, color, cumsum = FALSE, refresh = TRUE) %>%
			sg_add_edges_delay_p(edges, created_at, id, source, target, cumsum = FALSE, refresh = TRUE)
	})
}

shinyApp(ui, server)