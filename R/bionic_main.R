#' Prepares graph to be used for model computation
#'
#' @export
prepare_graph <- function(ion_type, base_graph_data, quart_base_graph_data, g,
                          g_quart){

  #duplicates the graph as it changes each run
  if(is.null(ion_type)){stop("No ION Type Column available")}
  else if(ion_type=="Q"){
    run_g_data <- rlang::duplicate(quart_base_graph_data)
    run_g <- rlang::duplicate(g_quart)
  }
  else{
    run_g_data <- rlang::duplicate(base_graph_data)
    run_g <- rlang::duplicate(g)
  }
  return(list("run_g_data"=run_g_data, "run_g"=run_g))
}

#' Run test and process the model graph ready for model computation
#'
#' Checks that:
#' - the data has columns of the model and not NA
#' - the data has no missing inputs
#' - the data is in the constraints of the parameters for the model
#' After checking it, it transforms the graph by removing unnecessary nodes
#' to the computation due to defined parameters in the data and creates a list
#' to hold the result data
#' @export
process_check_skip <- function(run_data, run_g, run_g_data){

  filled_run_names <- names(run_data)[which(!is.na(run_data))]
  nodes <- filled_run_names[filled_run_names %in% igraph::V(run_g)$name]

  #skips run if there is no data
  if (length(nodes)==0){
    return(list("skip"=T, "reason"="no_nodes"))
  }

  #create list to hold our result data for the run
  run_result_list <- as.list(run_data[nodes])

  #finds incoming edges for that vertex and removes edges for it
  incoming_edges <- igraph::E(run_g)[.to(nodes)]
  run_g <- igraph::delete_edges(run_g, incoming_edges)

  #removes nodes that are not needed for calculation
  run_g <- prune_graph(run_g, run_g_data$root)

  #checks if there are inputs missing and skips run if there are
  input_check <- input_checker(run_g_data, run_g, run_result_list)
  if(input_check$missing){
    return(list("skip"=T, "reason"="missing_input", "node"=input_check$node))
  }

  #checks if there any inputs that are out of the range for the parameter
  invalid_params <- constraint_checker(run_g_data, run_result_list)
  if(length(invalid_params)>0){
    return(list("skip"=T, "reason"="invalid_params", "params"=invalid_params))
  }

  return(list(skip=F, run_result_list=run_result_list, run_g=run_g))
}

#' Add defaults to the result list
#' @export
add_defaults_to_results <- function(run_result_list, run_g_data){

  default_nodes <- sapply(run_g_data$vertices, function(x) x[["type"]]=="default")
  default_names <- names(run_g_data$vertices)[default_nodes]
  default_names_add <- default_names[!default_names%in%names(run_result_list)]
  defaults_list <- lapply(run_g_data$vertices[default_names_add], function(x) x$value)
  run_result_list <- append(run_result_list, defaults_list)

  return(run_result_list)
}

#' Run BIONICv3 model on data
#'
#' Uses the BIONICv3 model as a graph based model to compute the BCF and any other parameters
#' needing to be calculated to get the BCF and returns the data. The graph based model is separated
#' into 2 where one model is for chemicals that are Neutral, Base or Acidic and the other is for QAC's
#' due to their different method of computing partition coefficients. It goes through each run/row of the data
#' and computes the data based on the input parameters available
#'
#' @param data A DataFrame which holds columns that have names of parameters in the BIONICv3 model
#' @return A list of the runs/rows of the chemicals with each item of each run being parameter and their value
#' @export
run_bionic_v3 <- function(data){

  runs <- nrow(data)
  results <- list()

  #creates the base data and the graph
  base_graph_data <- create_bionicv3_model()
  quart_base_graph_data <- create_bionicv3_quart_model()

  g <- igraph::graph_from_data_frame(base_graph_data$edges)
  g_quart <- igraph::graph_from_data_frame(quart_base_graph_data$edges)

  for (run in 1:runs){

    run_data <- data[run,]

    #prepare the graph data and graph model
    graph_prop <- prepare_graph(run_data$ion_type, base_graph_data,
                                quart_base_graph_data, g, g_quart)
    run_g <- graph_prop$run_g
    run_g_data <- graph_prop$run_g_data

    #test data against graph and create run result list
    check <- process_check_skip(run_data, run_g, run_g_data)
    if (check$skip){
      warning_msg <- switch(check$reason,
                        "no_nodes"=paste("The run", run, "has no data filled that corresponds to the model"),
                        "missing_input"=paste("The variable", check$node, "is either NA or not defined in run", run),
                        "invalid_params" = paste("The following variables do not follow constraints in run", run, ":",
                                                 paste(check$params, collapse = ", "))
                        )
      warning(warning_msg)
      next
    }
    run_result_list <- check$run_result_list
    run_g <- check$run_g

    #gets default values not added to the result list
    run_result_list <- add_defaults_to_results(run_result_list, run_g_data)

    #computes the model and gets all results
    run_result_list <- compute_graph(run_g_data, run_g, run_result_list)
    results[[run]] <- run_result_list

  }

  return(results)
}
