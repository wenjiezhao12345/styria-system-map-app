# R Shiny System Map App
# Interactive FCM / System Map Viewer
#
# Final version for Styria system map:
# - UK obesity example data removed
# - Default data comes from the Styria edge and node files
# - Audience can upload their own edge and node Excel/CSV files
# - Uploaded files do NOT apply automatically
# - Apply uploaded audience map button switches the whole app to audience data
# - Restore Styria original map button returns to the original Styria system map
# - Separate fixed 1-step / 2-step / 3-step tabs removed
# - Upstream and downstream use the sidebar step slider
# - Upstream focal node is placed at the bottom
# - Downstream focal node is placed at the top
# - Edge file supports: Source / Target / Description
# - Node file supports: Label / Description



# 0. Packages


packages <- c(
  "shiny",
  "visNetwork",
  "igraph",
  "dplyr",
  "readr",
  "readxl",
  "stringr",
  "tidyr",
  "shinyscreenshot"
)


library(shiny)
library(visNetwork)
library(igraph)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(shinyscreenshot)



# 1. Helper functions


standardise_edges <- function(df) {
  
  names(df) <- tolower(trimws(names(df)))
  
  from_candidates <- c(
    "from", "source", "start", "origin", "sender", "cause",
    "variable 1", "variable_1", "node1", "node_1"
  )
  
  to_candidates <- c(
    "to", "target", "end", "destination", "receiver", "effect",
    "variable 2", "variable_2", "node2", "node_2"
  )
  
  sign_candidates <- c(
    "sign", "polarity", "relationship", "relation", "weight",
    "effect_sign", "edge sign", "edge_sign", "description"
  )
  
  from_col <- intersect(from_candidates, names(df))[1]
  to_col <- intersect(to_candidates, names(df))[1]
  sign_col <- intersect(sign_candidates, names(df))[1]
  
  if (is.na(from_col) | is.na(to_col)) {
    stop("Your edge file must contain columns such as Source/Target or from/to.")
  }
  
  if (is.na(sign_col)) {
    df$sign <- "+"
    sign_col <- "sign"
  }
  
  edges <- df %>%
    transmute(
      from = as.character(.data[[from_col]]),
      to = as.character(.data[[to_col]]),
      sign = as.character(.data[[sign_col]])
    ) %>%
    filter(!is.na(from), !is.na(to), from != "", to != "") %>%
    mutate(
      from = str_squish(from),
      to = str_squish(to),
      sign = str_squish(sign),
      sign = case_when(
        tolower(sign) %in% c("+", "positive", "pos", "1", "increase", "increases") ~ "+",
        tolower(sign) %in% c("-", "negative", "neg", "-1", "decrease", "decreases") ~ "-",
        TRUE ~ sign
      )
    ) %>%
    distinct()
  
  return(edges)
}


read_map_file <- function(path) {
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "csv") {
    df <- read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(path)
  } else {
    stop("Please upload a CSV or Excel edge file.")
  }
  
  standardise_edges(df)
}


standardise_nodes <- function(df) {
  
  names(df) <- tolower(trimws(names(df)))
  
  id_candidates <- c("id", "node", "name", "label", "variable", "concept")
  label_candidates <- c("label", "name", "node", "variable", "concept")
  group_candidates <- c("group", "category", "type", "class", "description")
  policy_candidates <- c("policy", "is_policy", "policy_relevant")
  
  id_col <- intersect(id_candidates, names(df))[1]
  label_col <- intersect(label_candidates, names(df))[1]
  group_col <- intersect(group_candidates, names(df))[1]
  policy_col <- intersect(policy_candidates, names(df))[1]
  
  if (is.na(id_col)) {
    stop("Your node file must contain an identifier column such as Label, id, node, name, variable, or concept.")
  }
  
  nodes <- df %>%
    mutate(
      id = as.character(.data[[id_col]]),
      label = if (!is.na(label_col)) as.character(.data[[label_col]]) else as.character(.data[[id_col]]),
      group = if (!is.na(group_col)) as.character(.data[[group_col]]) else "Variable",
      policy = if (!is.na(policy_col)) as.character(.data[[policy_col]]) else "No"
    ) %>%
    transmute(
      id = str_squish(id),
      label = str_squish(label),
      group = str_squish(group),
      policy = str_squish(policy)
    ) %>%
    filter(!is.na(id), id != "") %>%
    distinct(id, .keep_all = TRUE)
  
  nodes$policy <- ifelse(
    tolower(nodes$policy) %in% c("yes", "y", "true", "1", "policy"),
    "Yes",
    "No"
  )
  
  return(nodes)
}


read_node_file <- function(path) {
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "csv") {
    df <- read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(path)
  } else {
    stop("Please upload a CSV or Excel node file.")
  }
  
  standardise_nodes(df)
}


make_graph <- function(edges, extra_nodes = NULL) {
  
  if (nrow(edges) == 0) {
    if (is.null(extra_nodes)) {
      return(make_empty_graph(directed = TRUE))
    } else {
      g <- make_empty_graph(directed = TRUE)
      g <- add_vertices(g, length(extra_nodes), name = extra_nodes)
      return(g)
    }
  }
  
  vertices <- unique(c(edges$from, edges$to, extra_nodes))
  vertices <- vertices[!is.na(vertices) & vertices != ""]
  
  graph_from_data_frame(
    edges,
    directed = TRUE,
    vertices = data.frame(name = vertices)
  )
}


make_subgraph_edges <- function(edges, nodes_keep) {
  edges %>%
    filter(from %in% nodes_keep, to %in% nodes_keep)
}


get_edges_df <- function(edges) {
  
  if (nrow(edges) == 0) {
    return(data.frame())
  }
  
  edges %>%
    transmute(
      from = from,
      to = to,
      label = "",
      title = paste0(from, " → ", to, " | polarity: ", sign),
      arrows = "to",
      color = ifelse(sign == "-", "#E53E3E", "#38A169"),
      dashes = ifelse(sign == "-", TRUE, FALSE),
      width = 2
    )
}


calculate_trophic_levels <- function(g) {
  
  node_names <- V(g)$name
  
  if (length(node_names) == 0) {
    return(data.frame(node = character(), level = numeric()))
  }
  
  indeg <- degree(g, mode = "in")
  source_nodes <- names(indeg[indeg == 0])
  
  if (length(source_nodes) == 0) {
    source_nodes <- node_names[1]
  }
  
  dist_matrix <- distances(
    g,
    v = source_nodes,
    to = node_names,
    mode = "out"
  )
  
  if (is.null(dim(dist_matrix))) {
    dist_vec <- as.numeric(dist_matrix)
    names(dist_vec) <- node_names
  } else {
    dist_vec <- apply(dist_matrix, 2, function(x) {
      if (all(is.infinite(x))) {
        NA
      } else {
        min(x[!is.infinite(x)])
      }
    })
  }
  
  finite_values <- dist_vec[is.finite(dist_vec) & !is.na(dist_vec)]
  
  if (length(finite_values) == 0) {
    dist_vec <- seq_along(node_names)
    names(dist_vec) <- node_names
  } else {
    dist_vec[is.na(dist_vec) | is.infinite(dist_vec)] <- max(finite_values) + 1
  }
  
  data.frame(
    node = names(dist_vec),
    level = as.numeric(dist_vec),
    stringsAsFactors = FALSE
  )
}


calculate_node_metrics <- function(edges) {
  
  g <- make_graph(edges)
  
  if (vcount(g) == 0) {
    return(data.frame())
  }
  
  trophic_df <- calculate_trophic_levels(g)
  
  metrics <- data.frame(
    Node = V(g)$name,
    Degree = as.numeric(degree(g, mode = "all")),
    In_degree = as.numeric(degree(g, mode = "in")),
    Out_degree = as.numeric(degree(g, mode = "out")),
    Closeness = round(as.numeric(closeness(g, mode = "all", normalized = TRUE)), 4),
    Betweenness = round(as.numeric(betweenness(g, directed = TRUE, normalized = TRUE)), 4),
    PageRank = round(as.numeric(page_rank(g, directed = TRUE)$vector), 4),
    stringsAsFactors = FALSE
  )
  
  metrics <- metrics %>%
    left_join(
      trophic_df %>%
        rename(Node = node, Trophic_level = level),
      by = "Node"
    ) %>%
    arrange(desc(Degree), desc(PageRank))
  
  return(metrics)
}


get_nodes_df <- function(g, focal_node = NULL, levels_df = NULL, node_info = NULL) {
  
  node_names <- V(g)$name
  
  nodes <- data.frame(
    id = node_names,
    label = node_names,
    title = node_names,
    stringsAsFactors = FALSE
  )
  
  if (!is.null(node_info)) {
    
    nodes <- nodes %>%
      left_join(node_info, by = "id") %>%
      mutate(
        label = ifelse(is.na(label.y), label.x, label.y),
        group = ifelse(is.na(group), "Variable", group),
        policy = ifelse(is.na(policy), "No", policy)
      ) %>%
      select(id, label, title, group, policy)
    
  } else {
    
    nodes$group <- "Variable"
    nodes$policy <- "No"
  }
  
  deg <- degree(g, mode = "all")
  nodes$value <- as.numeric(deg[nodes$id]) + 8
  nodes$shape <- "dot"
  
  nodes$color.background <- case_when(
    nodes$policy == "Yes" ~ "#F6AD55",
    tolower(nodes$group) %in% c("policy", "policy option", "intervention") ~ "#F6AD55",
    tolower(nodes$group) %in% c("outcome", "impact", "consequence") ~ "#BEE3F8",
    tolower(nodes$group) %in% c("actor", "stakeholder", "institution") ~ "#C6F6D5",
    TRUE ~ "#BFD7EA"
  )
  
  nodes$color.border <- case_when(
    nodes$policy == "Yes" ~ "#C05621",
    tolower(nodes$group) %in% c("policy", "policy option", "intervention") ~ "#C05621",
    TRUE ~ "#2B6CB0"
  )
  
  nodes$font.size <- 18
  nodes$font.align <- "center"
  
  nodes$title <- paste0(
    "<b>", nodes$label, "</b><br>",
    "ID: ", nodes$id, "<br>",
    "Group: ", nodes$group, "<br>",
    "Policy relevant: ", nodes$policy
  )
  
  if (!is.null(focal_node) && focal_node %in% nodes$id) {
    nodes$color.background[nodes$id == focal_node] <- "#FC8181"
    nodes$color.border[nodes$id == focal_node] <- "#C53030"
    nodes$value[nodes$id == focal_node] <- max(nodes$value, na.rm = TRUE) + 8
  }
  
  if (!is.null(levels_df)) {
    nodes <- nodes %>%
      left_join(levels_df, by = c("id" = "node"))
  }
  
  return(nodes)
}


render_vis_map <- function(
    edges,
    focal_node = NULL,
    layout_type = "full",
    title_text = "System map",
    node_info = NULL,
    extra_nodes = NULL
) {
  
  g <- make_graph(edges, extra_nodes = extra_nodes)
  
  if (vcount(g) == 0) {
    return(
      visNetwork(
        data.frame(id = "No data", label = "No data"),
        data.frame(),
        main = title_text,
        height = "650px",
        width = "100%"
      )
    )
  }
  
  levels_df <- NULL
  
  if (layout_type == "trophic") {
    
    levels_df <- calculate_trophic_levels(g)
    
  } else if (!is.null(focal_node) && focal_node %in% V(g)$name) {
    
    if (layout_type == "upstream") {
      
      dist_to_focal <- distances(g, to = focal_node, mode = "out")
      dist_vec <- as.numeric(dist_to_focal[, focal_node])
      names(dist_vec) <- rownames(dist_to_focal)
      dist_vec[is.infinite(dist_vec)] <- NA
      
      finite_dist <- dist_vec[is.finite(dist_vec) & !is.na(dist_vec)]
      
      if (length(finite_dist) == 0) {
        max_dist <- 0
      } else {
        max_dist <- max(finite_dist)
      }
      
      levels_df <- data.frame(
        node = names(dist_vec),
        level = max_dist - dist_vec,
        stringsAsFactors = FALSE
      ) %>%
        filter(!is.na(level))
      
      levels_df$level[levels_df$node == focal_node] <- max_dist + 1
      
    } else if (layout_type == "downstream") {
      
      dist_from_focal <- distances(g, v = focal_node, mode = "out")
      dist_vec <- as.numeric(dist_from_focal[focal_node, ])
      names(dist_vec) <- colnames(dist_from_focal)
      dist_vec[is.infinite(dist_vec)] <- NA
      
      levels_df <- data.frame(
        node = names(dist_vec),
        level = dist_vec,
        stringsAsFactors = FALSE
      ) %>%
        filter(!is.na(level))
      
      levels_df$level[levels_df$node == focal_node] <- 0
    }
  }
  
  nodes <- get_nodes_df(
    g,
    focal_node = focal_node,
    levels_df = levels_df,
    node_info = node_info
  )
  
  edges_vis <- get_edges_df(edges)
  
  net <- visNetwork(
    nodes,
    edges_vis,
    main = title_text,
    height = "700px",
    width = "100%"
  ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.9)),
      smooth = list(enabled = TRUE, type = "dynamic")
    ) %>%
    visNodes(
      font = list(size = 18, vadjust = 0),
      borderWidth = 2
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      selectedBy = "id"
    ) %>%
    visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      navigationButtons = TRUE,
      hover = TRUE
    )
  
  if (layout_type %in% c("upstream", "downstream") && !is.null(levels_df)) {
    
    net <- net %>%
      visHierarchicalLayout(
        enabled = TRUE,
        direction = "UD",
        sortMethod = "directed",
        levelSeparation = 170,
        nodeSpacing = 190,
        treeSpacing = 230
      ) %>%
      visPhysics(enabled = FALSE)
    
  } else if (layout_type == "trophic" && !is.null(levels_df)) {
    
    net <- net %>%
      visHierarchicalLayout(
        enabled = TRUE,
        direction = "LR",
        sortMethod = "directed",
        levelSeparation = 190,
        nodeSpacing = 180,
        treeSpacing = 220
      ) %>%
      visPhysics(enabled = FALSE)
    
  } else {
    
    net <- net %>%
      visPhysics(
        enabled = TRUE,
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -70,
          centralGravity = 0.02,
          springLength = 170,
          springConstant = 0.08
        ),
        stabilization = list(enabled = TRUE, iterations = 1000)
      )
  }
  
  return(net)
}


get_upstream_edges <- function(edges, focal_node, steps = 3) {
  
  g <- make_graph(edges)
  
  if (!(focal_node %in% V(g)$name)) {
    return(edges[0, ])
  }
  
  upstream_nodes <- ego(
    g,
    order = steps,
    nodes = focal_node,
    mode = "in"
  )[[1]] %>%
    names()
  
  make_subgraph_edges(edges, upstream_nodes)
}


get_downstream_edges <- function(edges, focal_node, steps = 3) {
  
  g <- make_graph(edges)
  
  if (!(focal_node %in% V(g)$name)) {
    return(edges[0, ])
  }
  
  downstream_nodes <- ego(
    g,
    order = steps,
    nodes = focal_node,
    mode = "out"
  )[[1]] %>%
    names()
  
  make_subgraph_edges(edges, downstream_nodes)
}


get_ego_edges <- function(edges, focal_node, steps = 1) {
  
  g <- make_graph(edges)
  
  if (!(focal_node %in% V(g)$name)) {
    return(edges[0, ])
  }
  
  ego_nodes <- ego(
    g,
    order = steps,
    nodes = focal_node,
    mode = "all"
  )[[1]] %>%
    names()
  
  make_subgraph_edges(edges, ego_nodes)
}


get_feedback_loops <- function(edges, min_len = 2, max_len = 6) {
  
  g <- make_graph(edges)
  
  if (vcount(g) == 0) {
    return(list())
  }
  
  all_cycles <- list()
  
  canonical_cycle <- function(x) {
    
    n <- length(x)
    
    rotations <- sapply(seq_len(n), function(i) {
      rotated <- c(x[i:n], x[seq_len(i - 1)])
      rotated <- rotated[rotated != ""]
      paste(rotated, collapse = " -> ")
    })
    
    sort(rotations)[1]
  }
  
  for (start_node in V(g)$name) {
    
    search_cycle <- function(current_node, path_nodes) {
      
      if (length(path_nodes) > max_len) {
        return(NULL)
      }
      
      next_nodes <- names(neighbors(g, current_node, mode = "out"))
      
      for (next_node in next_nodes) {
        
        if (next_node == start_node && length(path_nodes) >= min_len) {
          
          all_cycles[[length(all_cycles) + 1]] <<- path_nodes
          
        } else if (!(next_node %in% path_nodes) && length(path_nodes) < max_len) {
          
          search_cycle(
            current_node = next_node,
            path_nodes = c(path_nodes, next_node)
          )
        }
      }
      
      return(NULL)
    }
    
    search_cycle(
      current_node = start_node,
      path_nodes = start_node
    )
  }
  
  if (length(all_cycles) == 0) {
    return(list())
  }
  
  cycle_keys <- sapply(all_cycles, canonical_cycle)
  all_cycles <- all_cycles[!duplicated(cycle_keys)]
  
  return(all_cycles)
}


make_loop_title <- function(loop_nodes) {
  
  if (length(loop_nodes) <= 4) {
    paste(loop_nodes, collapse = " → ")
  } else {
    paste0(
      loop_nodes[1],
      " → ",
      loop_nodes[2],
      " → ... → ",
      loop_nodes[length(loop_nodes)]
    )
  }
}


get_loop_edges <- function(edges, loop_nodes) {
  
  loop_pairs <- data.frame(
    from = loop_nodes,
    to = c(loop_nodes[-1], loop_nodes[1]),
    stringsAsFactors = FALSE
  )
  
  edges %>%
    inner_join(loop_pairs, by = c("from", "to"))
}


make_loop_summary <- function(loops) {
  
  if (length(loops) == 0) {
    return(data.frame())
  }
  
  data.frame(
    Loop = seq_along(loops),
    Loop_name = sapply(loops, make_loop_title),
    Number_of_nodes = sapply(loops, length),
    Nodes_in_loop = sapply(loops, function(x) paste(x, collapse = " → ")),
    stringsAsFactors = FALSE
  )
}


get_shortest_path_edges <- function(edges, node_a, node_b) {
  
  g <- make_graph(edges)
  
  if (!(node_a %in% V(g)$name) | !(node_b %in% V(g)$name)) {
    return(edges[0, ])
  }
  
  path <- shortest_paths(
    g,
    from = node_a,
    to = node_b,
    mode = "all",
    output = "vpath"
  )$vpath[[1]]
  
  if (length(path) == 0) {
    return(edges[0, ])
  }
  
  path_nodes <- names(path)
  
  if (length(path_nodes) < 2) {
    return(edges[0, ])
  }
  
  path_pairs <- data.frame(
    from = path_nodes[-length(path_nodes)],
    to = path_nodes[-1],
    stringsAsFactors = FALSE
  )
  
  path_edges <- edges %>%
    inner_join(path_pairs, by = c("from", "to"))
  
  if (nrow(path_edges) < nrow(path_pairs)) {
    
    reverse_edges <- edges %>%
      rename(from_original = from, to_original = to) %>%
      mutate(from = to_original, to = from_original) %>%
      inner_join(path_pairs, by = c("from", "to")) %>%
      transmute(
        from = to_original,
        to = from_original,
        sign = sign
      )
    
    path_edges <- bind_rows(path_edges, reverse_edges) %>%
      distinct()
  }
  
  return(path_edges)
}



# 2. Load default Styria map files


possible_edge_files <- c(
  "Styria CM Edge List for Gephi 19 April 2024(in).csv",
  "Styria_CM_Edge_List_for_Gephi_19_April_2024_in.csv",
  "Styria CM Edge List for Gephi 19 April 2024(in).xlsx",
  "Styria_CM_Edge_List_for_Gephi_19_April_2024_in.xlsx",
  "Styria_CM_Edge.csv",
  "Styria_CM_Edges.csv",
  "edges.csv",
  "edges.xlsx"
)

possible_node_files <- c(
  "Styria CM Node List for Gephi 19 April 2024(in).csv",
  "Styria_CM_Node_List_for_Gephi_19_April_2024_in.csv",
  "Styria CM Node List for Gephi 19 April 2024(in).xlsx",
  "Styria_CM_Node_List_for_Gephi_19_April_2024_in.xlsx",
  "Styria_CM_Node.csv",
  "Styria_CM_Nodes.csv",
  "nodes.csv",
  "nodes.xlsx"
)

default_edge_file <- NULL
default_node_file <- NULL

for (f in possible_edge_files) {
  if (file.exists(f)) {
    default_edge_file <- f
    break
  }
}

for (f in possible_node_files) {
  if (file.exists(f)) {
    default_node_file <- f
    break
  }
}

if (!is.null(default_edge_file)) {
  original_edges <- read_map_file(default_edge_file)
} else {
  original_edges <- data.frame(
    from = character(),
    to = character(),
    sign = character(),
    stringsAsFactors = FALSE
  )
}

if (!is.null(default_node_file)) {
  original_nodes <- read_node_file(default_node_file)
} else {
  original_nodes <- NULL
}


# ---------------------------
# 3. UI
# ---------------------------

ui <- fluidPage(
  
  titlePanel("Interactive System Map Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Upload audience map files"),
      
      uiOutput("edge_upload_ui"),
      
      uiOutput("node_upload_ui"),
      
      actionButton(
        "apply_uploaded",
        "Apply uploaded audience map",
        icon = icon("upload")
      ),
      
      br(),
      br(),
      
      actionButton(
        "restore_default",
        "Restore Styria original map",
        icon = icon("rotate-left")
      ),
      
      br(),
      br(),
      
      helpText("Step 1: Upload the audience's edge file and node file."),
      helpText("Step 2: Click 'Apply uploaded audience map'. Then all maps and tables will change to the audience's own data."),
      helpText("Click 'Restore Styria original map' to return to the original Styria system map."),
      
      helpText("Edge file must contain at least: Source/from, Target/to, and Description/sign/polarity."),
      helpText("Node file should contain: Label/id/node/name and Description/group/category/type."),
      
      uiOutput("current_data_status"),
      
      hr(),
      
      h4("Map controls"),
      
      uiOutput("focal_node_ui"),
      
      sliderInput(
        "steps",
        "Number of steps",
        min = 1,
        max = 3,
        value = 1,
        step = 1
      ),
      
      hr(),
      
      h4("Screenshot"),
      
      screenshotButton(
        label = "Take screenshot",
        filename = "system_map_screenshot",
        selector = "body"
      ),
      
      hr(),
      
      h4("How to read the maps"),
      
      p("Green edges indicate positive relationships. Red dashed edges indicate negative relationships."),
      p("Orange nodes indicate policy-relevant nodes when this information is provided in the node file."),
      p("The interactive tabs allow dragging, zooming, selecting, and inspecting nodes.")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Interactive Full Map",
          br(),
          fluidRow(
            column(
              width = 8,
              visNetworkOutput("full_map", height = "700px")
            ),
            column(
              width = 4,
              h3("Explanation"),
              p("This interactive full system map shows all variables and all causal relationships in the dataset."),
              p("Each node represents a concept, policy factor, outcome, actor, or system variable."),
              p("Each directed arrow shows the direction of influence."),
              p("The number of steps slider on the left does not affect this full map. It only affects upstream, downstream, and ego-network views."),
              p("Highlight nearest means that when you click or hover over a node, its directly connected neighbouring nodes and edges are visually emphasised."),
              p("Select by id means that the dropdown search uses the unique node ID. This is useful when two nodes have similar labels but different identifiers."),
              p("You can drag nodes, zoom, and hover over edges to inspect the system structure.")
            )
          )
        ),
        
        tabPanel(
          "Interactive Trophic Map",
          br(),
          fluidRow(
            column(
              width = 8,
              visNetworkOutput("trophic_map", height = "700px")
            ),
            column(
              width = 4,
              h3("Explanation"),
              p("The interactive trophic map arranges variables according to causal hierarchy."),
              p("Nodes closer to the left are earlier upstream drivers or source variables."),
              p("Nodes further to the right are more downstream consequences."),
              p("This view helps explain the overall direction of causal influence in the system.")
            )
          )
        ),
        
        tabPanel(
          "Node metrics",
          br(),
          h3("Node metrics and centrality summary"),
          p("This table summarises each node's degree, in-degree, out-degree, closeness, betweenness, PageRank, and trophic level."),
          tableOutput("metrics_table")
        ),
        
        tabPanel(
          "Policy use",
          br(),
          fluidRow(
            column(
              width = 6,
              h3("Policy-relevant nodes"),
              p("This table lists nodes marked as policy-relevant in the uploaded node file."),
              p("A node is treated as policy-relevant if the node file has policy = Yes, is_policy = TRUE, or group/category/type = Policy."),
              tableOutput("policy_nodes_table")
            ),
            column(
              width = 6,
              h3("How this map supports policy justification"),
              p("The system map can be used to justify policy choices by showing how interventions are connected to wider system outcomes."),
              p("Policy nodes show where decision-makers may intervene in the system."),
              p("Upstream maps help identify causes that policy can target."),
              p("Downstream maps help explain likely consequences of a policy intervention."),
              p("Feedback loops show whether a policy may create reinforcing or balancing effects over time.")
            )
          )
        ),
        
        tabPanel(
          "Upstream map",
          br(),
          fluidRow(
            column(
              width = 8,
              visNetworkOutput("upstream_map", height = "700px")
            ),
            column(
              width = 4,
              h3("Explanation"),
              p("This upstream map uses the number of steps selected in the sidebar."),
              p("The focal node is placed at the bottom."),
              p("Variables above it are upstream causes or drivers."),
              p("Step 1 shows direct causes. Step 2 adds causes of causes. Step 3 gives a wider upstream causal structure."),
              p("Use this view to justify which causes or drivers should be targeted first.")
            )
          )
        ),
        
        tabPanel(
          "Downstream map",
          br(),
          fluidRow(
            column(
              width = 8,
              visNetworkOutput("downstream_map", height = "700px")
            ),
            column(
              width = 4,
              h3("Explanation"),
              p("This downstream map uses the number of steps selected in the sidebar."),
              p("The focal node is placed at the top."),
              p("Variables below it are downstream effects or consequences."),
              p("Step 1 shows direct consequences. Step 2 adds indirect consequences. Step 3 gives a wider downstream causal structure."),
              p("Use this view to explain the possible effects of an intervention or policy option.")
            )
          )
        ),
        
        tabPanel(
          "Ego networks",
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("ego_select_ui"),
              h3("Explanation"),
              p("An ego network shows the local neighbourhood around one selected node."),
              p("The app names each ego network using the focal node name."),
              p("This makes each local map easier to interpret."),
              p("The number of steps slider controls how wide the ego network becomes.")
            ),
            column(
              width = 8,
              visNetworkOutput("ego_map", height = "700px")
            )
          )
        ),
        
        tabPanel(
          "Feedback loop summary",
          br(),
          h3("Feedback loop summary table"),
          p("This table lists all detected feedback loops. Each loop is named using its actual node content rather than loop numbers only."),
          tableOutput("loop_summary_table")
        ),
        
        tabPanel(
          "Feedback loop viewer",
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("loop_select_ui"),
              h3("Explanation"),
              p("A feedback loop is a circular causal pathway where a chain of relationships eventually returns to the starting node."),
              p("Each loop title is generated from the actual nodes included in the loop."),
              p("This makes each loop easier to explain in the report.")
            ),
            column(
              width = 8,
              visNetworkOutput("loop_map", height = "700px")
            )
          )
        ),
        
        tabPanel(
          "Path between two nodes",
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("path_node_a_ui"),
              uiOutput("path_node_b_ui"),
              actionButton("show_path", "Show relationship path"),
              h3("Explanation"),
              p("This page displays the shortest relationship path between two selected nodes."),
              p("It helps explain how two concepts are connected in the system map."),
              p("This can support justification by showing the pathway between a policy option and a system outcome.")
            ),
            column(
              width = 8,
              visNetworkOutput("path_map", height = "700px")
            )
          )
        ),
        
        tabPanel(
          "Upload guidance",
          br(),
          fluidRow(
            column(
              width = 6,
              h3("What the edge file should look like"),
              p("The edge file should be a CSV or Excel file. It defines the causal relationships between variables."),
              p("Required columns:"),
              tags$ul(
                tags$li("Source/from: the starting node of the causal relationship"),
                tags$li("Target/to: the receiving node of the causal relationship"),
                tags$li("Description/sign/polarity: positive (+) or negative (-) relationship")
              ),
              p("Example edge file:"),
              tableOutput("edge_format_example")
            ),
            column(
              width = 6,
              h3("What the node file should look like"),
              p("The node file should be a CSV or Excel file. It provides extra information about each node."),
              p("Recommended columns:"),
              tags$ul(
                tags$li("Label/id/node: must match the node names used in the edge file"),
                tags$li("Description/group/category/type: for example Policy, Outcome, Driver, Actor")
              ),
              p("Example node file:"),
              tableOutput("node_format_example")
            )
          ),
          hr(),
          h3("Purpose of upload"),
          p("The upload function is used to visualise another system map and justify the structure of causal relationships."),
          p("It is not designed as a comparison tool. The app does not compare two maps or calculate similarity between maps."),
          p("After uploading both files, click Apply uploaded audience map to change all maps and tables to the audience's data."),
          p("The Restore Styria original map button returns the app to the original Styria system map.")
        )
      )
    )
  )
)


# ---------------------------
# 4. Server
# ---------------------------

server <- function(input, output, session) {
  
  map_state <- reactiveValues(
    edges = original_edges,
    nodes = original_nodes,
    data_source = "Styria original system map",
    upload_reset = 0
  )
  
  
  # Dynamic upload inputs
  # These are re-rendered when Restore is clicked.
  
  output$edge_upload_ui <- renderUI({
    
    map_state$upload_reset
    
    fileInput(
      "edge_file",
      "Upload audience edge file",
      accept = c(".csv", ".xlsx", ".xls")
    )
  })
  
  
  output$node_upload_ui <- renderUI({
    
    map_state$upload_reset
    
    fileInput(
      "node_file",
      "Upload audience node file",
      accept = c(".csv", ".xlsx", ".xls")
    )
  })
  
  
  # Apply uploaded audience files only after button click
  
  observeEvent(input$apply_uploaded, {
    
    if (is.null(input$edge_file) || is.null(input$node_file)) {
      showNotification(
        "Please upload both an edge file and a node file before applying the audience map.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    new_edges <- tryCatch(
      read_map_file(input$edge_file$datapath),
      error = function(e) {
        showNotification(
          paste("Edge file error:", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      }
    )
    
    new_nodes <- tryCatch(
      read_node_file(input$node_file$datapath),
      error = function(e) {
        showNotification(
          paste("Node file error:", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      }
    )
    
    if (is.null(new_edges) || is.null(new_nodes)) {
      return(NULL)
    }
    
    map_state$edges <- new_edges
    map_state$nodes <- new_nodes
    map_state$data_source <- "Audience uploaded map"
    
    showNotification(
      "Audience map applied. All maps and tables now use the uploaded data.",
      type = "message",
      duration = 5
    )
    
  })
  
  
  # Restore original Styria system map
  
  observeEvent(input$restore_default, {
    
    map_state$edges <- original_edges
    map_state$nodes <- original_nodes
    map_state$data_source <- "Styria original system map"
    map_state$upload_reset <- map_state$upload_reset + 1
    
    showNotification(
      "Restored to the original Styria system map.",
      type = "message",
      duration = 5
    )
    
  })
  
  
  edges_reactive <- reactive({
    map_state$edges
  })
  
  
  nodes_reactive <- reactive({
    map_state$nodes
  })
  
  
  output$current_data_status <- renderUI({
    
    edge_count <- nrow(edges_reactive())
    node_count_from_edges <- length(unique(c(edges_reactive()$from, edges_reactive()$to)))
    
    node_file_count <- if (is.null(nodes_reactive())) {
      "No node file loaded"
    } else {
      paste0(nrow(nodes_reactive()), " node rows loaded")
    }
    
    tags$div(
      style = "font-size: 13px; background-color: #F7FAFC; border: 1px solid #CBD5E0; padding: 8px; border-radius: 6px;",
      tags$strong("Current data: "),
      map_state$data_source,
      tags$br(),
      paste0("Edges: ", edge_count),
      tags$br(),
      paste0("Nodes in edge list: ", node_count_from_edges),
      tags$br(),
      node_file_count
    )
  })
  
  
  node_names_reactive <- reactive({
    
    node_names <- sort(unique(c(edges_reactive()$from, edges_reactive()$to)))
    
    if (length(node_names) == 0) {
      return("No data loaded")
    }
    
    node_names
  })
  
  
  output$focal_node_ui <- renderUI({
    
    selectInput(
      "focal_node",
      "Choose focal node",
      choices = node_names_reactive(),
      selected = node_names_reactive()[1]
    )
  })
  
  
  output$ego_select_ui <- renderUI({
    
    selectInput(
      "ego_node",
      "Choose ego network focal node",
      choices = node_names_reactive(),
      selected = node_names_reactive()[1]
    )
  })
  
  
  output$path_node_a_ui <- renderUI({
    
    selectInput(
      "path_node_a",
      "First node",
      choices = node_names_reactive(),
      selected = node_names_reactive()[1]
    )
  })
  
  
  output$path_node_b_ui <- renderUI({
    
    selectInput(
      "path_node_b",
      "Second node",
      choices = node_names_reactive(),
      selected = node_names_reactive()[min(2, length(node_names_reactive()))]
    )
  })
  
  
  # ---------------------------
  # Interactive full map
  # ---------------------------
  
  output$full_map <- renderVisNetwork({
    
    render_vis_map(
      edges = edges_reactive(),
      focal_node = NULL,
      layout_type = "full",
      title_text = "Full system map",
      node_info = nodes_reactive()
    )
  })
  
  

  # Interactive trophic map

  
  output$trophic_map <- renderVisNetwork({
    
    render_vis_map(
      edges = edges_reactive(),
      focal_node = NULL,
      layout_type = "trophic",
      title_text = "Trophic / causal hierarchy map",
      node_info = nodes_reactive()
    )
  })
  
  

  # Node metrics

  
  output$metrics_table <- renderTable({
    
    if (nrow(edges_reactive()) == 0) {
      return(data.frame(Message = "No edge data loaded. Please place the Styria edge CSV in the same folder or upload an edge file."))
    }
    
    calculate_node_metrics(edges_reactive())
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  

  # Policy nodes table

  
  output$policy_nodes_table <- renderTable({
    
    node_info <- nodes_reactive()
    
    if (is.null(node_info)) {
      return(data.frame(
        Message = "No node file loaded. Upload a node file with Description/group/category/type or policy/is_policy columns to identify policy-relevant nodes."
      ))
    }
    
    policy_nodes <- node_info %>%
      filter(
        policy == "Yes" |
          tolower(group) %in% c("policy", "policy option", "intervention")
      ) %>%
      select(id, label, group, policy)
    
    if (nrow(policy_nodes) == 0) {
      return(data.frame(
        Message = "No policy-relevant nodes found in the node file."
      ))
    }
    
    policy_nodes
    
  }, striped = TRUE, bordered = TRUE)
  
  

  # Upstream map
  # Uses sidebar step slider
  # Focal node at the bottom

  
  output$upstream_map <- renderVisNetwork({
    
    req(input$focal_node)
    
    up_edges <- get_upstream_edges(
      edges = edges_reactive(),
      focal_node = input$focal_node,
      steps = input$steps
    )
    
    render_vis_map(
      edges = up_edges,
      focal_node = input$focal_node,
      layout_type = "upstream",
      title_text = paste0(input$focal_node, " - ", input$steps, "-step upstream map"),
      node_info = nodes_reactive(),
      extra_nodes = input$focal_node
    )
  })
  
  

  # Downstream map
  # Uses sidebar step slider
  # Focal node at the top

  
  output$downstream_map <- renderVisNetwork({
    
    req(input$focal_node)
    
    down_edges <- get_downstream_edges(
      edges = edges_reactive(),
      focal_node = input$focal_node,
      steps = input$steps
    )
    
    render_vis_map(
      edges = down_edges,
      focal_node = input$focal_node,
      layout_type = "downstream",
      title_text = paste0(input$focal_node, " - ", input$steps, "-step downstream map"),
      node_info = nodes_reactive(),
      extra_nodes = input$focal_node
    )
  })
  
  

  # Ego network

  
  output$ego_map <- renderVisNetwork({
    
    req(input$ego_node)
    
    ego_edges <- get_ego_edges(
      edges = edges_reactive(),
      focal_node = input$ego_node,
      steps = input$steps
    )
    
    render_vis_map(
      edges = ego_edges,
      focal_node = input$ego_node,
      layout_type = "full",
      title_text = paste0("Ego network: ", input$ego_node),
      node_info = nodes_reactive(),
      extra_nodes = input$ego_node
    )
  })
  
  

#feedback_loop
  
  loops_reactive <- reactive({
    
    get_feedback_loops(
      edges = edges_reactive(),
      min_len = 2,
      max_len = 6
    )
  })
  
  
  output$loop_summary_table <- renderTable({
    
    loops <- loops_reactive()
    
    validate(
      need(length(loops) > 0, "No feedback loops found.")
    )
    
    make_loop_summary(loops)
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  output$loop_select_ui <- renderUI({
    
    loops <- loops_reactive()
    
    if (length(loops) == 0) {
      return(p("No feedback loops found."))
    }
    
    loop_titles <- sapply(loops, make_loop_title)
    
    selectInput(
      "selected_loop",
      "Choose feedback loop",
      choices = loop_titles,
      selected = loop_titles[1]
    )
  })
  
  
  output$loop_map <- renderVisNetwork({
    
    loops <- loops_reactive()
    
    validate(
      need(length(loops) > 0, "No feedback loops found.")
    )
    
    loop_titles <- sapply(loops, make_loop_title)
    selected_index <- match(input$selected_loop, loop_titles)
    
    if (is.na(selected_index)) {
      selected_index <- 1
    }
    
    loop_nodes <- loops[[selected_index]]
    loop_edges <- get_loop_edges(edges_reactive(), loop_nodes)
    
    render_vis_map(
      edges = loop_edges,
      focal_node = loop_nodes[1],
      layout_type = "full",
      title_text = paste0("Feedback loop: ", make_loop_title(loop_nodes)),
      node_info = nodes_reactive(),
      extra_nodes = loop_nodes
    )
  })
  
  
 
  # Path between two nodes

  
  path_edges_reactive <- eventReactive(input$show_path, {
    
    req(input$path_node_a, input$path_node_b)
    
    get_shortest_path_edges(
      edges = edges_reactive(),
      node_a = input$path_node_a,
      node_b = input$path_node_b
    )
  })
  
  
  output$path_map <- renderVisNetwork({
    
    req(input$path_node_a, input$path_node_b)
    
    path_edges <- path_edges_reactive()
    
    validate(
      need(nrow(path_edges) > 0, "No relationship path found between the selected nodes.")
    )
    
    render_vis_map(
      edges = path_edges,
      focal_node = input$path_node_a,
      layout_type = "full",
      title_text = paste0(
        "Relationship path: ",
        input$path_node_a,
        " to ",
        input$path_node_b
      ),
      node_info = nodes_reactive(),
      extra_nodes = c(input$path_node_a, input$path_node_b)
    )
  })
  
  

  # Upload guidance example tables

  
  output$edge_format_example <- renderTable({
    
    data.frame(
      Source = c("Variable A", "Variable B", "Variable C"),
      Target = c("Variable B", "Variable C", "Variable D"),
      Description = c("positive", "negative", "positive")
    )
    
  }, striped = TRUE, bordered = TRUE)
  
  
  output$node_format_example <- renderTable({
    
    data.frame(
      Label = c("Variable A", "Variable B", "Variable C"),
      Description = c("Driver", "Policy", "Outcome")
    )
    
  }, striped = TRUE, bordered = TRUE)
}



# 5. Run app


shinyApp(ui = ui, server = server)
