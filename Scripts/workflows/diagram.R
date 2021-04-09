

library(DiagrammeR)


DiagrammeR::grViz("

digraph boxes {

graph [layout = dot]

node [color = 'white']

1 -> 2 -> 3-> 4 -> 5 [color = 'white'] 

node [color = 'black', shape = rectangle, style = filled, fillcolor = Linen]

loc [label = '1 User location + \n travel distance']
filt [label = '2 Filter user \n preferences']
sdm [label = '3 Process SDMs + \n create metric']
access [label = '3* Filter accessible \n areas']
combin [label = '4 Overlay access \n and metric']
fin [label = '5 Final nudge', shape = box]
feed [label = '5* Inform \n future nudges']

loc -> filt [lhead = cluster1]
# feed -> filt [lhead = cluster1]



subgraph cluster1{
filt -> sdm;
filt -> access; 
access -> combin; 
sdm -> combin; 
combin -> fin;

}

feed -> filt [constraint = F, style = 'dashed', color = 'red'];
fin -> feed [constraint = F, style = 'dashed', color = 'red'];

subgraph{
rank = 'same'; fin; feed
}



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

