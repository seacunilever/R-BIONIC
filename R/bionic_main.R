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
process_check_skip <- function(nodes, run_g, run_g_data, run_result_list){

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

topo_order <- function(run_g, run_result_list){

  #gets the topological order of the graph
  g_order <- names(igraph::topo_sort(run_g))

  #removes already filled values from the order
  g_order <- g_order[!g_order%in%names(run_result_list)]

  return(g_order)
}

#' Run graph
#'
#' @param data A DataFrame which holds columns that have names of parameters in the model
#' @param g_data A list including the root, edges dataframe and vertices list
#' @return A list of results for each row
#' @export
run_graph <- function(data, g_data){

  g <- igraph::graph_from_data_frame(g_data$edges)

  #creates data for quicker computer times
  funcs <- g_data$vertices[sapply(g_data$vertices, function(x)x$type=="function")]
  inc_edges <- igraph::incident_edges(g, names(funcs), mode = "in")
  compute_data <- lapply(names(funcs), function(x){list(param_names = inc_edges[[x]]$param_name,
                                                        vertex_params = names(igraph::tail_of(g, inc_edges[[x]])),
                                                        func_name = g_data$vertices[[x]]$value
                                                        )})
  names(compute_data)<-names(funcs)

  runs <- nrow(data)
  results <- list()
  memory <- list()

  for (run in 1:runs){

    run_data <- data[run,]

    filled_run_names <- names(run_data)[which(!is.na(run_data))]
    nodes <- filled_run_names[filled_run_names %in% igraph::V(g)$name]

    #skips run if there is no data
    if (length(nodes)==0){
      warning(paste("The run", run, "has no data filled that corresponds to the model"))
      next
    }

    #create list to hold our result data for the run
    run_result_list <- as.list(run_data[nodes])

    memory_code <- paste(which(colnames(data) %in% nodes), collapse="_")

    if (is.null(memory[[memory_code]])){

      #prepare the graph model
      run_g <- rlang::duplicate(g)

      #test data against graph and create run result list
      check <- process_check_skip(nodes, run_g, g_data, run_result_list)
      if (check$skip){
        warning_msg <- switch(check$reason,
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
      run_result_list <- add_defaults_to_results(run_result_list, g_data)

      g_order <- topo_order(run_g, run_result_list)

      #works of the principle that the same inputs have the same topological order
      memory[[memory_code]] <- g_order

    }

    #gets default values not added to the result list
    run_result_list <- add_defaults_to_results(run_result_list, g_data)

    #computes the model and gets all results
    run_result_list <- compute_graph(compute_data, memory[[memory_code]], run_result_list)
    results[[run]] <- run_result_list
  }
  return(results)
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

  results <- list()

  if(is.null(data$ion_type)){stop("No ION Type Column available")}

  if (sum(data[,"ion_type"]!="Q")>0){

    data_non_q <- data[data[,"ion_type"]!="Q",]

    base_graph_data <- create_bionicv3_model()
    results[as.numeric(rownames(data_non_q))] <- run_graph(data_non_q, base_graph_data)
  }
  if (sum(data[,"ion_type"]=="Q")>0){

    data_q <- data[data[,"ion_type"]=="Q",]

    quart_base_graph_data <- create_bionicv3_quart_model()
    results[as.numeric(rownames(data_q))] <- run_graph(data_q, quart_base_graph_data)
  }

  return(results)
}
