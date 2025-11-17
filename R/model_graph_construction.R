#'Creates base graph data
#'
#'This is a starting function that is used to create staring graph data that is used
#'throughout the operation functions and the main function.
#'
#'@param root A string which is assigned as the name of the root of graph which will be the
#'the final variable to be calculated
#' @param root_function A string which represents a function name in the global environment
#' that is used to calculate the root
#' @param custom_param_names A vector containing the names that assign different names to the parameters for the function in the
#' root_function.Has to be equal to the number of parameters is the actual function if used.
#'This is used when a general function is used so if it used regularly, the variables that are used to
#' call the function change names, therefore this allows the change of variable names.
#' @return list of data items for a base graph, which includes a root, vertices list and edges dataframe
#' @exportPattern create
create_base_graph <- function(root, root_function, custom_param_names = NA){

  if(!exists(root_function)){
    stop("The defined root function does not exist")
  }

  if (all(is.na(custom_param_names))){
    custom_param_names <- formalArgs(root_function)
  }

  if (length(custom_param_names)!=length(formalArgs(root_function))){
    stop("The number of names in custom_param_names is higher than the number of inputted parameter names for the function")
  }

  edges <- data.frame(from = custom_param_names,
                        to = rep(root, length(custom_param_names)),
                        param_name = formalArgs(root_function)[1:length(custom_param_names)])

  vertices <- list()
  vertices[[root]] <- list("name"=root,
                           "type"="function",
                           "value"=root_function)

  g_data <- list(root = root, edges=edges, vertices=vertices)

  return(g_data)
}

#' Add model to graph data
#'
#' This adds a model onto existing graph data. It is only able to add a model if the
#' output parameter is already defined as being used in the edges dataframe but not in the vertices.
#' This ensures that the added model is connected with the rest of the graph.
#'
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param vertex_name A string that is the vertex name to be defined as the model which exists in the edges dataframe
#' @param vertex_type A string that can either be 'function'/'f' or 'default'/'d', where a model based on a function is added
#' if 'function'/'f' is chosen otherwise if a 'default'/'d' is chosen then only a value is added for the vertex
#' @param vertex_value Either a value if the vertex is default or a name of a function in the global environment if the vertex is a function
#' @param custom_param_names A vector containing the names that assign different names to the parameters for the function
#' @return A g_data list with the added model
#' @exportPattern add
add_model_to_graph <- function(g_data, vertex_name, vertex_type, vertex_value, custom_param_names = NA){

  if(!any(vertex_type%in%c("function", "default", "f", "d"))){
    stop("The vertex_type has to be either 'function'/'f' or 'default'/'d' ")
  }

  if(vertex_type == "function"|| vertex_type=="f"){
    if(!exists(vertex_value)){
      stop("The defined vertex function does not exist")
    }
    #uses default parameter names if none defined
    if (any(is.na(custom_param_names))){
      custom_param_names <- formalArgs(vertex_value)
    }
    if (length(custom_param_names)>length(formalArgs(vertex_value))){
      stop("The number of names in custom_param_names is higher than the number of inputted parameter names for the function")
      }
  }

  if(!any(g_data$edges$from == vertex_name)){
    stop("The vertex_name does not exist in the current graph")
  }

  if(any(g_data$edges$to == vertex_name)){
    stop("The vertex name has model already defined, please either use delete_model_graph to delete it before adding the
         model in the graph or set_model_graph to change the model in graph")
  }

  #if a new function is added it needs edges to the parameters
  if (vertex_type == "function"||vertex_type == "f"){
    new_edge_df <- data.frame(from = custom_param_names,
                              to = rep(vertex_name, length(custom_param_names)),
                              param_name = formalArgs(vertex_value)[1:length(custom_param_names)])
    g_data$edges <- rbind(g_data$edges, new_edge_df)

    g_data$vertices[[vertex_name]] <- list("name" = vertex_name,
                                           "type" = "function",
                                           "value" = vertex_value)
  }else{
    g_data$vertices[[vertex_name]] <- list("name" = vertex_name,
                                           "type" = "default",
                                           "value" = vertex_value)
  }

  return(g_data)
}

#' Add multiple defaults to the graph data
#'
#' This allows to add multiple default values to the model. This is used when multiple
#' default values are wanted to be added without iterating over \link[BIONICv3]{add_model_graph}
#'
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param default_names A vector of strings, These are parameter values that are already in the g_data(specifically
#' the edges dataframe) that will be defined as defaults(specifically vertex list)
#' @param default_values A vector of values, that correspond to the value that will be assigned to the named default names. Note: if using
#' different data types for default values, it is recommended to do multiple instances of this function separated by the data types
#' @return A g_data list with the added defaults
add_defaults_to_graph <- function(g_data, default_names, default_values){

  if(!is.atomic(default_names) || !is.atomic(default_values)){
    stop("default names and default values have to be both vectors")
  }

  if (length(default_names)!=length(default_values)){
    stop("The number of names given and the values given is not equal")
  }

  all_vertices <- unique(c(g_data$edges$from, g_data$edges$to))
  default_vertices_bool <- default_names%in%all_vertices

  if (sum(default_vertices_bool)!=length(default_names)){
    warning("Some defaults given are not currently in the graph")
  }

  #creates a list of the default parameters to the form of the list
  new_vertex_list <- setNames(lapply(seq_along(default_names),
                                     function(i){
                                       list("name" = default_names[i],
                                            "type" = "default",
                                            "value" = default_values[i])
                                     }),
                              default_names)

  g_data$vertices <- append(g_data$vertices, new_vertex_list)

  return(g_data)
}

#' Add inputs to graph
#'
#' This function adds input to graph based on which parameters have been defined in the edges but not
#' the vertices. It uses these parameters and adds them to the vetices list as inputs without a value.
#' You can think this as closing the graph data
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @return A g_data list including the inputs in the vertices list
add_inputs_to_graph <- function(g_data){

  g_inputs <- unique(g_data$edges$from[!g_data$edges$from %in% names(g_data$vertices)])

  for (inp in g_inputs){
    g_data$vertices[[inp]]<- list("name"=inp,
                                  "type"="input")
  }

  return(g_data)
}

#' Add constraint to parameter in graph
#'
#' Adds a constraint to a parameter in a graph based on either being a choice or a range.
#'
#' If it is a range than the following lexicon '[0,9];(12,36)neq 4, 5' which based on 2 parts
#' the inequality part and the not equal part. The ineqaulity parts follow the closed/open intervals where brackets are
#' exclusive and square brackets are inclusive, the inequalities are seperated with a semi colon(;)
#' The not equal part starts off with a 'neq ' and any numbers following it seperated by a comma(,)
#' are not included in the interval.
#'
#' If a choice parameter is chosen, then a vector of values is needed
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param vertex_name a string that is the vertex that is inside the vertices and edges
#' @param constraint_type a string between 'range' and 'choice'
#' @param constraint The data type that is described above in the description to create the constraint for the parameter
#' @return A list g_data including the addition of a constraint inside the vertices for a given parameter
add_constraint_to_graph <- function(g_data, vertex_name, constraint_type, constraint){

  if(!constraint_type%in%c("range", "choice")){
    stop("Constraint type has to be either range or choice")
  }
  if (!vertex_name %in% names(g_data$vertices)){
    stop("The vertex is not in the vertex list please check with names(g_data$vertices)")

  }
  if(constraint_type=="choice"){
    if (!is.vector(constraint) || length(constraint)<=1){
      stop("For choice the constraint has to be a vector and have at least two choices")
    }
    g_data$vertices[[vertex_name]]$constraint_type <- "choice"
    g_data$vertices[[vertex_name]]$constraint <- constraint
  }
  else{
    if(!is.character(constraint)){
      stop("For range the constraint have to be a string")
    }

    #checks the form of the interval is right
    split <- unlist(strsplit(constraint, "neq "))
    regex_pattern <- "(\\[|\\(){1}(\\d+(\\.\\d+)?)*,(\\d+(\\.\\d+)?)*(\\]|\\)){1}"
    for (i in unlist(strsplit(split[1], ";"))){
      if (is.na(regmatches(i,regexpr(regex_pattern, i))[1])){
        stop("One of the intervals is not in the form of [|(d+,d+)|]")
      }
    }

    g_data$vertices[[vertex_name]]$constraint_type <- "range"
    g_data$vertices[[vertex_name]]$constraint <- constraint
  }

  return(g_data)
}

#' Change a model in graph
#'
#' This is when the graph data contains a parameter that has a function applied to it and it needs to be replaced by another function
#' To replace the function, the replacing function has to have the same parameter names and output due to keeping the structure of the
#' graph. If instead different parameters are wanted, please use \link[BIONICv3]{delete_model_from_graph} then \link[BIONICv3]{add_model_to_graph}
#'
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param verte_name a string that is the vertex that is inside the vertices and edges, the vertex that the function will be changing its function
#' @param func_name A string that is the new function name to replace the older one, has to be in the global environment
#' @param param_names_map A list that represents a map from existing parameter names to new ones, if just the name of the parameters has changed
#' for the function
#' @return A list g_data with the changed function for the certain vertex chosen
#' @exportPattern change
change_model_in_graph <- function(g_data, vertex_name, func_name, param_names_map = NA){

  if(!any(names(g_data$vertices)==vertex_name)){
    stop("that vertex name is not in the graph data")
  }
  if(g_data$vertices[[vertex_name]]$type!="function"){
    stop("The vertex is not a function but instead is a default/input")
  }

  if(!exists(func_name)){
    stop("Cant find an object/function with the func_name in the environment, please make sure it is loaded in")
  }

  vertex_cur_params <- g_data$edges[g_data$edges$to==vertex_name, "param_name"]

  if(all(is.na(param_names_map))){

    if(sum(formalArgs(func_name)%in%vertex_cur_params)!=length(vertex_cur_params) ||
       length(formalArgs(func_name))!=length(vertex_cur_params)){
      stop("The function does not have the same parameter names as the current function set for the vertex")
    }else{
      g_data$vertices[[vertex_name]]$value <- func_name
    }
  }
  else{

    if(!is.list(param_names_map)){
      stop("The param_names_map has to be a list as key=value")
    }
    match_names <- names(param_names_map)[names(param_names_map)%in%vertex_cur_params]
    if(length(param_names_map)!=length(match_names)){
      stop("Some keys in the param_names_map do not match with the current function parameters")
    }
    if(sum(param_names_map[match_names]%in%formalArgs(func_name))!=length(match_names)){
      stop("the values in the param_names_map_list do not equal with the arguments in the new function")
    }
    non_match_names <- vertex_cur_params[!vertex_cur_params%in%match_names]
    if(sum(non_match_names%in%formalArgs(func_name))!=length(non_match_names)){
      stop("The current function parameters that were not defined in param_names_map were not found in the new function")
    }
    #maps the new parameter names in the edges dataframe
    for (name in match_names) {
      g_data$edges[g_data$edges$to==vertex_name & g_data$edges$param_name==name, "param_name"] <- param_names_map[[name]]
    }
    g_data$vertices[[vertex_name]]$value <- func_name
  }

  return(g_data)
}

#' Replace a named function in the whole graph data
#'
#' Iterates over \link[BIONICv3]{change_model_in_graph} to replace old named function with a valid new function
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param old_function a string that represents the name of the function to replace in the model
#' @param new_function a string that represents the name of the function that will replace the old function. This has to be loaded to the global
#' environment
#' @param param_name A list that represents a map from existing parameter names to new ones, if just the name of the parameters has changed
#' for the function
#' @return A list g_data with the changed function for the all the vertices with the old function
change_function_in_graph <- function(g_data, old_function, new_function, param_names_map = NA){

  verts <- names(g_data$vertices[sapply(g_data$vertices, function(x)x$type=="function"&x$value==old_function)])

  for (ver in verts) {
    g_data<-change_model_in_graph(g_data, ver, new_function, param_names_map)
  }
  return(g_data)
}

#' Change default value in a graph
#'
#' Changes the default value of a certain vertex
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param vertex_name A string that represents the name of the vertex/parameter that value will be changes. Has to be a default value
#' @param default_value A value that will be replacing the old default value
#' @return A list g_data with a replaced default value
change_default_in_graph <- function(g_data, vertex_name, default_value){

  if(!any(names(g_data$vertices)==vertex_name)){
    stop("that vertex name is not in the graph data")
  }
  if(g_data$vertices[[vertex_name]]$type!="default"){
    stop("The vertex is not a default but instead is a function")
  }
  g_data$vertices[[vertex_name]]$value <- default_value

  return(g_data)
}

#' Delete model and any incoming connections from graph
#'
#' From a chosen model it will delete the model from the graph by removing any
#' incoming edges from the graph and any function/defualt associated with the parameter/vertex
#' Then it prunes any vertices that are not needed anymore due to deletion. This allows for new models
#' with different parameters to be able to be added to the model
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param vertex_name A string representing the vertex/parameter that will have it's model deleted and any incoming edges
#' @return A list of g_data with the model removed for vertex_name
#' @exportPattern delete
delete_model_from_graph <- function(g_data, vertex_name){

  if(!any(names(g_data$vertices)==vertex_name)){
    stop("that vertex name is not in the graph data")
  }

  if(g_data$vertices[[vertex_name]]$type=="input"){
    stop("The vertex is a input, these can not be individually deleted, please use delete_inputs_from_graph instead")
  }

  #uses pruning to delete the model and other unneeded sub-models due to deletion
  if(g_data$vertices[[vertex_name]]$type=="function"){

    g <- igraph::graph_from_data_frame(g_data$edges)

    #gets the incoming parameters needed for the model and deletes the edges
    incoming_edges <- igraph::E(g)[.to(vertex_name)]
    g <- igraph::delete_edges(g, incoming_edges)

    #deletes all vertices that are not connected to the graph
    g <- prune_graph(g, g_data$root)

    g_data$edges <- igraph::as_data_frame(g)
    vertices_left <- c(names(igraph::V(g)))
    g_data$vertices <- g_data$vertices[names(g_data$vertices)%in%vertices_left]

  }
  g_data$vertices <- g_data$vertices[names(g_data$vertices)!=vertex_name]

  return(g_data)
}

#' Removes inputs from the graph data
#'
#' removes the type in vertices that are inputs. THis function is used to open the graph
#' data again
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#'
delete_inputs_from_graph <- function(g_data){

  g_data$vertices <- g_data$vertices[sapply(g_data$vertices, function(x)x$type!="input")]

  return(g_data)
}

#' Join 2 graph data's
#'
#' This joins the root of the right to the graph on the left. The purpose of this
#' is to join a main graph with a sub graph
#' @param left A list g_data containing the root of the right g_data. This will be the main graph
#' @param right A list g_data where the root is part of the left g_data.This will be the sub graph
#' @param override A boolean, where if False it only uses the leaves of the left g_data to connect to the
#' right g_data. Otherwise if True it uses any node
#' @return A list g_data with both graphs combined
#' @export
join_graphs <- function(left, right, override=F){

  if(sum(left$edges$from==right$root)!=1){
    stop("The root of the right graph is not a vertex in the left graph")
  }

  if(!override && sum(names(left$vertices)==right$root)==1){
    stop("The root of the right is not a leaf in the left, you can overide this by
         setting override to TRUE")
  }

  #removes instances of the right rot in the left if override is true
  if(override){
    left <- delete_model_from_graph(left, right$root)
  }

  #gets matching vertex and removes them from the right data before adding to the left g_data
  matching_vertex <- names(right$vertices)[names(right$vertices)%in%names(left$vertices)]

  for (ver in matching_vertex) {
    warning(paste0("The vertex ", ver, " is in both graphs so it will be deleted in the right one"))
    right <- delete_model_from_graph(right, ver)
  }
  left$edges <- rbind(left$edges, right$edges)
  left$vertices <- append(left$vertices, right$vertices)

  return(left)
}

