

library(DiagrammeR)


DiagrammeR::grViz("

digraph boxes {

graph [layout = dot]
node [shape = rectangle, style = filled, fillcolor = Linen]

loc [label = 'User location + \n distance to travel']
access [label = 'Filter accessible areas']
sdm [label = 'Process SDMs + \n create metric']
combin [label = 'Overlay access \n and variation', shape = box]
filt [label = 'Filter user \n preferences', shape = box]
fin [label = 'Final nudge', shape = box]
feed [label = 'Feedback for \n future nudges']

loc -> filt -> {access sdm} -> combin -> fin 
fin -> feed -> filt

subgraph {
rank = same; feed; filt;
}

}
")

graph_1 <- 
  create_graph() %>%
  add_node() %>%
  select_nodes_by_id(nodes = 1) %>%
  add_n_nodes_ws(
    n = 5,
    direction = "from"
  ) %>%
  add_n_nodes_ws(
    n = 5,
    direction = "to"
  )

graph_1 %>% render_graph()


g2 <- 
  create_graph() %>% 
  add_node() %>% 
  select_nodes_by_id(nodes = 1) %>% 
  add_n_nodes_ws(n = 2,
                 direction = 'from')
g2 %>% render_graph()

