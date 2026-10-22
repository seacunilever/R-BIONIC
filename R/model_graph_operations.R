#' Checks inputs are valid in run data
#'
#' checks if the inputs nodes that are needed for the run are defined and not NA. This accounts for
#' any for  pruned inputs.
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param g An igraph object that has the orignal inputs pruned based on the run parameters
#' @param run_result_list The current run result list
#' @return A list with a boolean of missing that depends on if there missing inputs in the run
#' results and node element that includes the missing node
#' @export
input_checker <-function(g_data, g, run_result_list){

  #finds nodes that have 0 incoming edges in g and that are also labelled "input" in g_data
  nodes_with_no_incoming <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]$name
  input_vertices <- names(which(sapply(g_data$vertices, function(x)x$type=="input")))
  input_nodes <- nodes_with_no_incoming[nodes_with_no_incoming %in% input_vertices]

  #checks if node is missing/NA
  for (node in input_nodes) {
    if(!node%in%names(run_result_list) || is.na(run_result_list[[node]])){
      return(list(missing=T, node=node))
    }
  }
  return(list(missing=F))
}

#' check data is included in the range
#'
#' checks if the range checker for an interval is in bounds (returns TRUE) or not(returns FALSE)
#' @param data A vector of numeric values or numeric values that will be checked based on the constraint
#' @param constraint A string that has the following lexicon '\[0,9\];(12,36)neq 4, 5' which based on 2 parts
#' the inequality part and the not equal part. The inequality parts follow the closed/open intervals where brackets are
#' exclusive and square brackets are inclusive, the inequalities are separated with a semi colon(;)
#' The not equal part starts off with a 'neq ' and any numbers following it separated by a comma(,)
#' are not included in the interval.
#' @return A Boolean if values are inside constraint then TRUE is returned, otherwise FALSE
#' @export
constraint_range_checker <- function(data, constraint) {

  split <- stringr::str_split(constraint, "neq ")[[1]]

  #False constraint if one of the numbers in neq
  not_equals <- as.numeric(unlist(stringr::str_split(split[2], ",")))
  if (any(data%in%not_equals)){return(FALSE)}

  #goes to each interval and check if numbers are at least in 1 of them
  intervals <- unlist(stringr::str_split(split[1],";"))
  interval_bool<-c()
  for (iter in intervals) {
    start <- substr(iter,1,1)
    end <- substr(iter, nchar(iter), nchar(iter))
    nums <- unlist(stringr::str_split(substr(iter, 2, nchar(iter)-1), ","))

    if (nums[1]!=""){

      #changes based on interval
      left_comparison <- all(switch (start,
                                     "[" = as.numeric(nums[1])<= data,
                                     "(" = as.numeric(nums[1])<data)
      )
    }
    else{left_comparison<-T}

    if(nums[2]!=""){
      right_comparison <- all(switch (end,
                                      "]" = data<=as.numeric(nums[2]),
                                      ")" = data<as.numeric(nums[2]))
      )
    }
    else{
      right_comparison <-T
    }
    interval_bool<-c(interval_bool, left_comparison&right_comparison)
  }

  if(any(interval_bool)){
    return(TRUE)
  }else{return(FALSE)}

}

#' Checks data is bounded by constraint
#'
#' for a given data it checks the constraints in g_data for the parameters and returns
#' parameters that do not fit with the constraint
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param data A list of the run data to be inspected by the constraint
#' @return A vector of the parameters that do not fit the constraint
#' @export
constraint_checker <- function(g_data, data) {

  if (!is.list(data)){stop("the data has to be a list type with names of parameters")}

  #gets parameters that are in data and has a constraint in g_data
  match_params <- names(g_data$vertices)[names(g_data$vertices) %in% names(data)]
  rel_params <- g_data$vertices[match_params]
  graph_con_params <- sapply(rel_params, function(x)!is.null(x$constraint_type))
  constraint_params <- names(rel_params[graph_con_params])

  invalid_params <- c()

  for (param in constraint_params){
    if(g_data$vertices[[param]]$constraint_type=="choice"){

      #checks if all the data is one of the choices
      if (!all(data[[param]]%in%g_data$vertices[[param]]$constraint)){
        invalid_params <- c(invalid_params, param)
      }
    }
    else{
      #checks if all the data is inside the defined interval
      if(!constraint_range_checker(data[[param]], g_data$vertices[[param]]$constraint)){
        invalid_params <- c(invalid_params, param)
      }
    }
  }
  return(invalid_params)
}

#' Prunes graph of unneeded vertices/parameters
#'
#' Goes through the process of removing the vertices and their incoming edges
#' which have 0 outgoing edges iteratively until there are no vertices with 0 outgoing
#' edges(except from the root)
#' @param g An igraph object of the graph with edges removed due to the input
#' @param root A string representing the root of the graph such that it is not removed from the graph
#' for having 0 outgoing edges
#' @return An igraph object of the graph with 0 vertices that have 0 outgoing edges(excpet from the root)
#' @export
prune_graph <- function(g, root){

  #finds nodes that have no outgoing edges except from the root
  outstanding_nodes <- igraph::V(g)[igraph::degree(g, mode = "out")==0 &
                                      igraph::V(g)$name!=root]

  while (length(outstanding_nodes)!=0) {

    #deleting vertices also deletes edges, so we can check for the degree property again and again
    g <- igraph::delete_vertices(g, outstanding_nodes)
    outstanding_nodes <- igraph::V(g)[igraph::degree(g, mode = "out")==0 &
                                        igraph::V(g)$name!=root]
  }

  return(g)
}

#' Computes graph through its topological order
#'
#' First it computes the topoligical order of the parameters/vertices and computes
#' the vertices in that order using the functions named in the vertices list
#' @param g_data A list including the root, edges dataframe and vertices list. Initiated by \link[BIONICv3]{create_base_graph}
#' @param g An igraph object that has been pruned and is a Directed Acyclic graph containing parameters as vertices
#' @param run_result_list The run result list that contains all the parameters that were inputted
#' @return A list of the run_result_list with the calculated parameters after running through the graph until the root
#' @export
compute_graph <- function(compute_data, g_order, run_result_list){

  for (node in g_order){

    func_dat <- setNames(run_result_list[compute_data[[node]]$vertex_params],
                         compute_data[[node]]$param_names)
    #compute using node function and the function
    run_result_list[[node]] <- do.call(compute_data[[node]]$func_name, func_dat)
  }
  return(run_result_list)
}
