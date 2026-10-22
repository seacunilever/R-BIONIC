library(igraph)
source("fixtures/funcs.R")

g_data<- list("root" = "test",
              "edges"= data.frame("from"=c("param_one", "param_two"),
                                  "to"=c("test", "test"),
                                  "param_name"=c("param_one","param_two")),
              "vertices" = list("test"=list("name"="test",
                                            "type"="function",
                                            "value"="example_func"))
)

g_data_closed <- list("root" = "test",
                      "edges"= data.frame("from"=c("param_one", "param_two"),
                                          "to"=c("test", "test"),
                                          "param_name"=c("param_one","param_two")),
                      "vertices" = list("test"=list("name"="test",
                                                    "type"="function",
                                                    "value"="example_func"),
                                        "param_one"=list("name"="param_one",
                                                         "type"="input",
                                                         "constraint_type"="choice",
                                                         "constraint"= c("A", "B", "C")),
                                        "param_two"=list("name"="param_two",
                                                         "type"="input",
                                                         "constraint_type"="range",
                                                         "constraint"="[0,10]neq 4,5")))
g <- igraph::graph_from_data_frame(g_data_closed$edges)

g_data_extented <- list("root" = "test",
              "edges"= data.frame("from"=c("param_one", "param_two", "cool_param", "lonely_param"),
                                  "to"=c("test", "test", "param_one", "cool_param"),
                                  "param_name"=c("param_one","param_two", "cool_param", "lonely_param")),
              "vertices" = list("test"=list("name"="test",
                                            "type"="function",
                                            "value"="example_func"),
                                "param_one"=list("name"="param_one",
                                                 "type"="function",
                                                 "value"="calc_param_one"),
                                "param_two"=list("name"="param_two",
                                                 "type"="default",
                                                 "value"=53),
                                "cool_param"=list("name"="cool_param",
                                                  "type"="function",
                                                  "value"="lonely_func"))
)

g_data_ext_closed <- add_inputs_to_graph(g_data_extented)

g_ext <- igraph::graph_from_data_frame(g_data_ext_closed$edges)

#-----Base-Graph------------------------------------
test_that("Test out a base graph:using valid/invalid functions",{

  #check if function does not exist
  expect_error(create_base_graph("test", "non_existing_func"))

  actual_list <- create_base_graph("test", "example_func")
  expect_equal(names(actual_list), c("root", "edges", "vertices"))
  expect_equal(actual_list$root, "test")
  expect_equal(actual_list$edges, data.frame("from"=c("param_one", "param_two"),
                                             "to"=c("test", "test"),
                                             "param_name"=c("param_one","param_two")))
  expect_equal(names(actual_list$vertices), c("test"))
  expect_equal(actual_list$vertices$test, list("name"="test",
                                               "type"="function",
                                               "value"="example_func"))
})

test_that("Test out a base graph: Test custom parameters",{

  #have more parameters than needed
  expect_error(create_base_graph("test", "example_func", c("1","2", "3")))

  #have less than needed parameters
  expect_error(create_base_graph("test", "example_func", c("1")))

  #test using different parameters
  actual_list <- create_base_graph("test", "example_func", c("test_one", "test_two"))
  expect_equal(names(actual_list), c("root", "edges", "vertices"))
  expect_equal(actual_list$root, "test")
  expect_equal(actual_list$edges, data.frame("from"=c("test_one", "test_two"),
                                             "to"=c("test", "test"),
                                             "param_name"=c("param_one","param_two")))
  expect_equal(names(actual_list$vertices), c("test"))
  expect_equal(actual_list$vertices$test, list("name"="test",
                                               "type"="function",
                                               "value"="example_func"))
})

#-----Adding-to-model-------------------------------------------------

test_that("Test adding a model to a graph: valid data",{

  #testing out function
  actual_list <- add_model_to_graph(g_data=g_data, vertex_name = "param_one", vertex_type="function",
                                    vertex_value = "calc_param_one")
  expect_equal(names(actual_list$vertices), c("test", "param_one"))
  expect_equal(actual_list$edges, data.frame("from"=c("param_one", "param_two", "cool_param"),
                                             "to"=c("test", "test", "param_one"),
                                             "param_name"=c("param_one","param_two", "cool_param")))

  expect_equal(actual_list$vertices$param_one, list("name"="param_one",
                                                    "type"="function",
                                                    "value"="calc_param_one"))

  #testing out defaults
  actual_list_def <- add_model_to_graph(g_data=g_data, vertex_name = "param_one", vertex_type="d",
                                         vertex_value = 1)
  expect_equal(names(actual_list_def$vertices), c("test", "param_one"))
  expect_equal(actual_list_def$edges, data.frame("from"=c("param_one", "param_two"),
                                             "to"=c("test", "test"),
                                             "param_name"=c("param_one","param_two")))
  expect_equal(actual_list_def$vertices$param_one, list("name"="param_one",
                                                    "type"="default",
                                                    "value"=1))
})

test_that("Test adding a model to a graph: invalid vertex types",{
  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "param_one", vertex_type="fanction",
                                  vertex_value = "calc_param_one"))
  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "param_one", vertex_type="g",
                                  vertex_value = "calc_param_one"))
})

test_that("Test adding a model to a graph: invalid vertex name",{

  #param_three does not exist
  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "param_three", vertex_type="function",
                                  vertex_value = "calc_param_one"))
  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "param_three", vertex_type="d",
                                  vertex_value = 1))
})

test_that("Test adding a model to a graph: invalid value",{

  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "param_one", vertex_type="function",
                                  vertex_value = "fake_func"))

  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "param_one", vertex_type="function",
                                  vertex_value = 123))
})

test_that("Test adding a model to a graph: adding to existing model", {

  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "test", vertex_type="f",
                                  vertex_value = "calc_param_one"))

  expect_error(add_model_to_graph(g_data=g_data, vertex_name = "test", vertex_type="d",
                                  vertex_value = "test"))
})

#-----Adding-multiple-defaults-----------------------------------------------

test_that("Test for adding multiple defaults to graph data: Valid data", {

  default_names <- c("param_one", "param_two")
  default_values <- c("cool", "cooler")

  actual_list_str_def <- add_defaults_to_graph(g_data, default_names, default_values)
  expect_equal(names(actual_list_str_def$vertices), c("test", "param_one", "param_two"))
  expect_equal(actual_list_str_def$vertices$param_one$type, "default")
  expect_equal(actual_list_str_def$vertices$param_one$value, "cool")
  expect_equal(actual_list_str_def$vertices$param_two$type, "default")
  expect_equal(actual_list_str_def$vertices$param_two$value, "cooler")

  default_names <- c("param_one", "param_two")
  default_values <- c(3.14, 600000000)

  actual_list_num_def <- add_defaults_to_graph(g_data, default_names, default_values)
  expect_equal(names(actual_list_num_def$vertices), c("test", "param_one", "param_two"))
  expect_equal(actual_list_num_def$vertices$param_one$type, "default")
  expect_equal(actual_list_num_def$vertices$param_one$value, 3.14)
  expect_equal(actual_list_num_def$vertices$param_two$type, "default")
  expect_equal(actual_list_num_def$vertices$param_two$value, 600000000)

  actual_list_one <- add_defaults_to_graph(g_data, default_names = "param_one", default_values = c(1))
  expect_equal(names(actual_list_one$vertices), c("test", "param_one"))
  expect_equal(actual_list_one$vertices$param_one$type, "default")
  expect_equal(actual_list_one$vertices$param_one$value, 1)
})

test_that("Test for adding multiple defaults to graph data: invalid names and values", {

  #test on invalid default names and values
  expect_error(add_defaults_to_graph(g_data, default_names = list("param_one", "param_two"), default_values = c(1,2)))
  expect_error(add_defaults_to_graph(g_data, default_names = c("param_one", "param_two"), default_values = list(1,2)))

  #inconsistent number of default names vs values
  expect_error(add_defaults_to_graph(g_data, default_names = c("param_one", "param_two"), default_values = c("test")))

  #fake default values
  expect_warning(add_defaults_to_graph(g_data, default_names = c("not_real", "param_one"), default_values = c("test", 2)))
})

#-----Adding-inputs-to-models-------------------------------------------------
test_that("Test for adding inputs to graph data", {

  expected_result <- list("root" = "test",
                             "edges"= data.frame("from"=c("param_one", "param_two"),
                                                 "to"=c("test", "test"),
                                                 "param_name"=c("test_one","test_two")),
                             "vertices" = list("test"=list("name"="test",
                                                           "type"="function",
                                                           "value"="example_func"),
                                               "param_one"=list("name"="param_one",
                                                                "type"="input"),
                                               "param_two"=list("name"="param_two",
                                                                "type"="input"))
  )

  actual_list <- add_inputs_to_graph(g_data)
  expect_equal(names(actual_list$vertices), c("test","param_one", "param_two"))
  expect_equal(actual_list$vertices$param_one$type, "input")
  expect_equal(actual_list$vertices$param_two$type, "input")
})

#-----Adding-constraints-to-model---------------------------------------------

test_that("Test adding constrainsts:Valid choice constraint is added correctly", {
  result <- add_constraint_to_graph(g_data, "test", "choice", c("A", "B", "C"))
  expect_equal(result$vertices$test$constraint_type, "choice")
  expect_equal(result$vertices$test$constraint, c("A", "B", "C"))
})

test_that("Test adding constrainsts:Valid range constraint is added correctly", {
  result <- add_constraint_to_graph(g_data, "test", "range", "[0,9];(12,36)neq 4,5")
  expect_equal(result$vertices$test$constraint_type, "range")
  expect_equal(result$vertices$test$constraint, "[0,9];(12,36)neq 4,5")
})

test_that("Test adding constrainsts:Invalid constraint type throws error", {
  expect_error(add_constraint_to_graph(g_data, "test", "invalid_type", c("A", "B")),
               "Constraint type has to be either range or choice")

  expect_error(add_constraint_to_graph(g_data, "test", "ronge", "(34,]new 4,5"),
               "Constraint type has to be either range or choice")
})

test_that("Test adding constrainsts:Invalid vertex throws error", {
  expect_error(add_constraint_to_graph(g_data, "invalid_vertex", "choice", c("A", "B")))
})

test_that("Test adding constrainsts:Choice constraint with non-empty-vector throws error", {
  expect_error(add_constraint_to_graph(g_data, "test", "choice", "not_a_vector"),
               "For choice the constraint has to be a vector and have at least two choices")

  expect_error(add_constraint_to_graph(g_data, "test", "choice", c()),
               "For choice the constraint has to be a vector and have at least two choices")
})

test_that("Test adding constrainsts:invalid range constraint with non-string throws error", {

  expect_error(add_constraint_to_graph(g_data, "test", "range", c(1, 2)),
               "For range the constraint have to be a string")
  expect_error(add_constraint_to_graph(g, "test", "range", "[0,9];12,36)neq 4,5"))

  expect_error(add_constraint_to_graph(g, "test", "range", "[0.34,9]];(12,36)neq 4,5"))

  expect_error(add_constraint_to_graph(g, "test", "range", "[0.34,9];(12,36)neq 4;5"))
})

#---Change-function--of--vertex------------------------------------------------
test_that("Test for changing vertex function: Valid function replacement", {
  actual_list <- change_model_in_graph(g_data, "test", "change_func")
  expect_equal(actual_list$vertices$test$value, "change_func")

  #with mapping
  param_map <- list("param_one" = "param_three", "param_two" = "param_four")

  actual_list_change <- change_model_in_graph(g_data, "test", "change_func_new_param", param_map)
  expect_equal(actual_list_change$vertices$test$value, "change_func_new_param")
  expect_equal(actual_list_change$edges$param_name, c("param_three", "param_four"))
  expect_equal(actual_list_change$edges$from, c("param_one", "param_two"))


  #with semi mapping
  param_map <- list("param_two" = "change_param")

  actual_list_semi_change <- change_model_in_graph(g_data, "test", "change_func_one_param", param_map)
  expect_equal(actual_list_semi_change$vertices$test$value, "change_func_one_param")
  expect_equal(actual_list_semi_change$edges$param_name, c("param_one", "change_param"))
  expect_equal(actual_list_semi_change$edges$from, c("param_one", "param_two"))

})

test_that("Test for changing vertex function: Invalid vertex name and function", {
  expect_error(change_model_in_graph(g_data, "missing_vertex", "change_func"),
               "that vertex name is not in the graph data")

  expect_error(change_model_in_graph(g_data, "test", "nonexistent_func"),
               "Cant find an object/function with the func_name in the environment")

  expect_error(change_model_in_graph(g_data, "test", "invalid_func"),
               "The function does not have the same parameter names as the current function set for the vertex")

  expect_error(change_model_in_graph(g_data, "test", "invalid_func_two"),
               "The function does not have the same parameter names as the current function set for the vertex")
})

test_that("Test for changing vertex function: Vertex is not a function type", {
  g_data$vertices$test$type <- "input"
  expect_error(change_model_in_graph(g_data, "test", "new_func"),
               "The vertex is not a function but instead is a default/input")

  g_data$vertices$test$type <- "default"
  expect_error(change_model_in_graph(g_data, "test", "new_func"),
               "The vertex is not a function but instead is a default/input")
  g_data$vertices$test$type <- "function"  # Reset for other tests
})

test_that("Test for changing vertex function: invalid param_names_map", {

  #param_names_map is not a list
  expect_error(change_model_in_graph(g_data, "test", "change_func", c("param_one", "param_two")),
               "The param_names_map has to be a list as key=value")

  #param_names_map keys do not match current parameters
  param_map <- list("wrong_param" = "param_one")
  expect_error(change_model_in_graph(g_data, "test", "change_func", param_map),
               "Some keys in the param_names_map do not match with the current function parameters")

  #param_names_map values do not match new function arguments
  param_map <- list("param_one" = "x", "param_two" = "y")
  expect_error(change_model_in_graph(g_data, "test", "change_func", param_map),
               "the values in the param_names_map_list do not equal with the arguments in the new function")

})

#----change-the-same-named-function-in-graph-data-------------------------------

test_that("Test for changing a function throughout graph", {

  g_data <- list(
    root = "test",
    edges = data.frame(
      from = c("param_one", "param_two", "param_one", "param_two"),
      to = c("test", "test", "test2", "test2"),
      param_name = c("param_one", "param_two", "param_one", "param_two"),
      stringsAsFactors = FALSE
    ),
    vertices = list(
      test = list(name = "test", type = "function", value = "example_func"),
      test2 = list(name = "test2", type = "function", value = "example_func")
    )
  )

  #Valid function replacement
  result <- change_function_in_graph(g_data, "example_func", "change_func")
  expect_equal(result$vertices$test$value, "change_func")
  expect_equal(result$vertices$test2$value, "change_func")

  #Valid function replacement with param_names_map
  param_map <- list("param_one" = "param_three", "param_two" = "param_four")
  actual_list_change <- change_function_in_graph(g_data, "example_func", "change_func_new_param", param_map)
  expect_equal(actual_list_change$vertices$test$value, "change_func_new_param")
  expect_equal(actual_list_change$vertices$test2$value, "change_func_new_param")
  expect_equal(actual_list_change$edges$param_name, c("param_three", "param_four", "param_three", "param_four"))
  expect_equal(actual_list_change$edges$from, c("param_one", "param_two", "param_one", "param_two"))

})

#---Change-the-defaults-of-vertex---------------------------------------------
test_that("Test to change defaults: Successfully changes default value", {
  g_data_test <- list("root" = "test",
                "edges"= data.frame("from"=c("param_one", "param_two"),
                                    "to"=c("test", "test"),
                                    "param_name"=c("param_one","param_two")),
                "vertices" = list("test"=list("name"="test",
                                              "type"="function",
                                              "value"="example_func"),
                                  "param_one"=list("name"="param_one",
                                                    "type"="default",
                                                    "value" = 3.14))
  )

  actual_list <- change_default_in_graph(g_data_test, "param_one", 42)
  expect_equal(actual_list$vertices$param_one$value, 42)
})

test_that("Test to change defaults: Invalid vertex and type", {
  expect_error(change_default_in_graph(g_data, "missing_vertex", 42),
               "that vertex name is not in the graph data")
  expect_error(change_default_in_graph(g_data, "test", 42),
               "The vertex is not a default but instead is a function")
})

#---delete-model-from-graph-data--------------------------------------------

test_that("Test to delete model from graph: Successfully deletes model", {

  #no dependencies
  actual_list <- delete_model_from_graph(g_data_extented, "cool_param")
  expect_false("cool_param" %in% names(actual_list$vertices))
  expect_false(any(actual_list$edges$to == "cool_param"))
  expect_false(any(actual_list$edges$from == "lonely_param"))

  #pruning involved
  actual_list_pruned <- delete_model_from_graph(g_data_extented, "param_one")
  expect_false("param_one" %in% names(actual_list_pruned$vertices))
  expect_false(any(actual_list_pruned$edges$to == "param_one"))
  expect_false("cool_param" %in% names(actual_list_pruned$vertices))
  expect_false(any(actual_list_pruned$edges$to == "cool_param"))
  expect_false(any(actual_list_pruned$edges$from == "cool_param"))
  expect_false(any(actual_list_pruned$edges$from == "lonely_param"))

  #removing default
  actual_list_def <- delete_model_from_graph(g_data_extented, "param_two")
  expect_false("param_two" %in% names(actual_list_def$vertices))
  expect_true(any(actual_list_def$edges$from == "param_two"))
})

test_that("Test to delete model from graph: invalid vertex name in graph", {

  #no vertex name within graph
  expect_error(delete_model_from_graph(g_data, "nonexistent"), "that vertex name is not in the graph data")

  #vertex is an input and not a default or model
  g_data_extented$vertices$param_two$type<-"input"
  expect_error(delete_model_from_graph(g_data_extented, "param_two"))
  g_data_extented$vertices$param_two$type<-"default"
})

#---Joining-graphs---------------
#TO do in future not fully fleshed out

#---Input-Checker------------------------------------------------------------

test_that("Test to check that input data is valid: Valid input",{

  #test on a graph that has not been pruned
  run_result_list <- list("param_one" =1, "param_two"=2)
  actual_list <- input_checker(g_data_closed, g, run_result_list)
  expect_equal(actual_list$missing, F)
  expect_equal(actual_list$node, c())

  #test on a graph that has been pruned
  run_result_list <- list("param_one" =1, "param_two"=2)
  actual_list <- input_checker(g_data_ext_closed, g, run_result_list)
  expect_equal(actual_list$missing, F)
  expect_equal(actual_list$node, c())

  #test on a graph that has been pruned, such that input values are not needed
  run_result_list <- list()
  actual_list <- input_checker(g_data_ext_closed, g, run_result_list)
  expect_equal(actual_list$missing, F)
  expect_equal(actual_list$node, c())

})

test_that("Test to check that input data is valid: Missing Inputs",{

  #test on a graph that has not been pruned, no inputs
  run_result_list <- list()
  actual_list <- input_checker(g_data_closed, g, run_result_list)
  expect_equal(actual_list$missing, T)
  expect_equal(actual_list$node, c("param_one"))

  #test on a graph that has not been pruned, one input
  run_result_list <- list("param_one" =1)
  actual_list <- input_checker(g_data_closed, g, run_result_list)
  expect_equal(actual_list$missing, T)
  expect_equal(actual_list$node, c("param_two"))
})

test_that("Test to check that input data is valid: NA inputs",{

  #test on a graph that has not been pruned
  run_result_list <- list("param_one" =1, "param_two"=NA)
  actual_list <- input_checker(g_data_closed, g, run_result_list)
  expect_equal(actual_list$missing, T)
  expect_equal(actual_list$node, c("param_two"))

  #test on a graph that has been pruned, and as these will not be inputs it does not matter
  run_result_list <- list("lonely_param" =NA)
  actual_list <- input_checker(g_data_ext_closed, g, run_result_list)
  expect_equal(actual_list$missing, F)
  expect_equal(actual_list$node, c())
})

#----Constraint-range-checker------------------------------------------------

test_that("Test Constraint Range Checker: exclusive and inclusive ranges",{

  expect_true(constraint_range_checker(c(5), "[0,10]"))
  expect_true(constraint_range_checker(c(0, 10), "[0,10]"))
  expect_false(constraint_range_checker(c(10.1), "[0,10]"))

  expect_true(constraint_range_checker(c(999999), "[0,]"))
  expect_false(constraint_range_checker(c(-1000), "[0,]"))
  expect_true(constraint_range_checker(c(0), "[,]"))

  expect_true(constraint_range_checker(c(5), "(0,10)"))
  expect_true(constraint_range_checker(c(9.9), "(0,10)"))
  expect_false(constraint_range_checker(c(0, 10), "(0,10)"))

  expect_true(constraint_range_checker(c(-99999), "(,10)"))

  expect_true(constraint_range_checker(c(2.5, 10), "(0,10]"))
  expect_true(constraint_range_checker(c(0, 9.9), "[0,10)"))
})

test_that("Test Constraint Range Checker: Not equal tests",{
  expect_false(constraint_range_checker(c(4), "[0,10]neq 4"))
  expect_true(constraint_range_checker(c(3), "[0,10]neq 4"))
  expect_false(constraint_range_checker(c(4, 5), "[0,10]neq 4,5"))

  })

test_that("Test Constraint Range Checker: Multiple intervals",{
  expect_true(constraint_range_checker(c(13), "[0,9];(12,36)"))
  expect_false(constraint_range_checker(c(11), "[0,9];(12,36)"))
})

#-----Constraint-Checker--------------------------------------------------
test_that("Test Constraint Checker: All parameter meets constraints",{
  data <- list("param_one"="A", "param_two"=6)
  expect_equal(constraint_checker(g_data_closed, data), c())
})

test_that("Test Constraint Checker: Constraint Violations",{
  data <- list("param_one"="Z", "param_two"=6)
  expect_equal(constraint_checker(g_data_closed, data), c("param_one"))

  data <- list("param_one"="A", "param_two"=4)
  expect_equal(constraint_checker(g_data_closed, data), c("param_two"))

  data <- list("param_one"="A", "param_two"=11)
  expect_equal(constraint_checker(g_data_closed, data), c("param_two"))

  data <- list("param_one"="Z", "param_two"=11)
  expect_equal(constraint_checker(g_data_closed, data), c("param_one", "param_two"))
})

test_that("Test Constraint Checker: No constraints", {
  data <- list("test"="6")
  expect_equal(constraint_checker(g_data_closed, data), c())
})

#-----Prune--Graph---------------------------------------------------

test_that("Test Pruning Graphs:Graph with no pruning needed", {
  g <- igraph::graph_from_data_frame(data.frame(from = c("A", "B"), to = c("C", "C")))
  V(g)$name <- c("A", "B", "C")
  pruned <- prune_graph(g, root = "C")
  expect_equal(sort(V(pruned)$name), sort(c("A", "B", "C")))
})

test_that("Test Pruning Graphs:Graph with one removable leaf", {
  g <- igraph::graph_from_data_frame(data.frame(from = c("A", "B"), to = c("C", "C")))
  V(g)$name <- c("A", "B", "C")
  g<- igraph::delete_edges(g, "A|C")
  pruned <- prune_graph(g, root = "C")
  expect_equal(V(pruned)$name, c("B", "C"))
})

test_that("Test Pruning Graphs:Graph with multiple removable leaves", {
  g <- graph_from_data_frame(data.frame(from = c("A", "B", "C"), to = c("D", "D", "D")))
  V(g)$name <- c("A", "B", "C", "D")
  g<- igraph::delete_edges(g, c("A|D","B|D"))
  pruned <- prune_graph(g, root = "D")
  expect_equal(V(pruned)$name, c("C", "D"))
})

test_that("Test Pruning Graphs:Graph with nested pruning", {
  g <- graph_from_data_frame(data.frame(from = c("A", "B", "C"), to = c("B", "C", "D")))
  V(g)$name <- c("A", "B", "C", "D")
  g <- igraph::delete_edges(g, c("B|C"))
  pruned <- prune_graph(g, root = "D")
  expect_equal(V(pruned)$name, c("C","D"))

  g <- graph_from_data_frame(data.frame(from = c("A", "B", "C", "A"), to = c("B", "C", "D", "D")))
  V(g)$name <- c("A", "B", "C", "D")
  g <- igraph::delete_edges(g, c("B|C"))
  pruned <- prune_graph(g, root = "D")
  expect_equal(V(pruned)$name, c("A","C","D"))
})

test_that("Test Pruning Graphs:Root node is preserved", {
  g <- graph_from_data_frame(data.frame(from = character(0), to = character(0)))
  g <- add_vertices(g, 1, name = "root")
  pruned <- prune_graph(g, root = "root")
  expect_equal(V(pruned)$name, "root")
})


#----Compute-Graph--------------------------------------------------

test_that("Test computing graph: No pruning graph computing",{

  funcs <- g_data_closed$vertices[sapply(g_data_closed$vertices, function(x)x$type=="function")]
  inc_edges <- igraph::incident_edges(g, names(funcs), mode = "in")
  compute_data <- lapply(names(funcs), function(x){list(param_names = inc_edges[[x]]$param_name,
                                                        vertex_params = names(igraph::tail_of(g, inc_edges[[x]])),
                                                        func_name = g_data_closed$vertices[[x]]$value
  )})
  names(compute_data)<-names(funcs)

  result_run_list <- list("param_one"=1, "param_two"="A")
  topo_order <- topo_order(g, result_run_list)

  actual_list <- compute_graph(compute_data, topo_order, result_run_list)
  expect_equal(actual_list, list("param_one"=1, "param_two"="A", "test"=1))

  funcs <- g_data_ext_closed$vertices[sapply(g_data_ext_closed$vertices, function(x)x$type=="function")]
  inc_edges <- igraph::incident_edges(g_ext, names(funcs), mode = "in")
  compute_data <- lapply(names(funcs), function(x){list(param_names = inc_edges[[x]]$param_name,
                                                        vertex_params = names(igraph::tail_of(g_ext, inc_edges[[x]])),
                                                        func_name = g_data_ext_closed$vertices[[x]]$value
  )})
  names(compute_data)<-names(funcs)

  result_run_list <- list("lonely_param"=42,"param_two"=53)
  topo_order <- topo_order(g_ext, result_run_list)

  actual_list <- compute_graph(compute_data, topo_order, result_run_list)
  expect_equal(actual_list, list("lonely_param"=42,"param_two"=53, "cool_param"=42, "param_one"=42, "test"=42))

})

test_that("Test computing graph: After Pruning", {

  g_pruned <- igraph::delete_vertices(g, c("param_one", "param_two"))

  funcs <- g_data$vertices[sapply(g_data$vertices, function(x)x$type=="function")]
  inc_edges <- igraph::incident_edges(g_pruned, names(funcs), mode = "in")
  compute_data <- lapply(names(funcs), function(x){list(param_names = inc_edges[[x]]$param_name,
                                                        vertex_params = names(igraph::tail_of(g_pruned, inc_edges[[x]])),
                                                        func_name = g_data$vertices[[x]]$value
  )})
  names(compute_data)<-names(funcs)
  result_run_list <- list("test"=1)
  topo_order <- topo_order(g_pruned, result_run_list)
  actual_list <- compute_graph(compute_data, topo_order, result_run_list)
  expect_equal(actual_list, list("test"=1))

  g_ext_pruned <- igraph::delete_vertices(g_ext, c("lonely_param", "cool_param"))
  funcs <- g_data$vertices[sapply(g_data$vertices, function(x)x$type=="function")]
  inc_edges <- igraph::incident_edges(g_ext_pruned, names(funcs), mode = "in")
  compute_data <- lapply(names(funcs), function(x){list(param_names = inc_edges[[x]]$param_name,
                                                        vertex_params = names(igraph::tail_of(g_ext_pruned, inc_edges[[x]])),
                                                        func_name = g_data$vertices[[x]]$value
  )})
  names(compute_data)<-names(funcs)
  result_run_list <- list("param_one"=42, "param_two"=61)
  topo_order <- topo_order(g_ext_pruned, result_run_list)
  actual_list <- compute_graph(compute_data, topo_order, result_run_list)
  expect_equal(actual_list, list("param_one"=42, "param_two"=61, "test"=42))
})
