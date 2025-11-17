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
#' @exportPattern calc
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

    #duplicates the graph as it changes each run
    if(is.null(run_data$ion_type)){stop("No ION Type Column available")}
    else if(run_data$ion_type=="Q"){
      run_g_data <- rlang::duplicate(quart_base_graph_data)
      run_g <- rlang::duplicate(g_quart)
    }
    else{
      run_g_data <- rlang::duplicate(base_graph_data)
      run_g <- rlang::duplicate(g)
    }

    filled_run_names <- names(run_data)[which(!is.na(run_data))]
    nodes <- filled_run_names[filled_run_names %in% igraph::V(run_g)$name]

    #skips run if there is no data
    if (length(nodes)==0){
      warning("The run ", run, " has no data filled that corresponds to the model")
      next
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
      warning("The variable ", input_check$node, " is either NA or not defined column in the input data for run ",
                  run)
      next
    }

    #checks if there any inputs that are out of the range for the parameter
    invalid_params <- constraint_checker(run_g_data, run_result_list)
    if(length(invalid_params)>0){
      warning(paste("The following variables do not follow the constraints in run ", run, " so they are skipped: ",
                    paste(invalid_params, collapse = ", ")))
      next
    }

    #gets default values not added to the result list
    default_names <- names(run_g_data$vertices)[sapply(run_g_data$vertices, function(x) x[["type"]]=="default")]
    default_names_add <- default_names[!default_names%in%names(run_result_list)]
    defaults_list <- lapply(run_g_data$vertices[default_names_add], function(x) x$value)
    run_result_list <- append(run_result_list, defaults_list)

    #computes the model and gets all results
    run_result_list <- compute_graph(run_g_data, run_g, run_result_list)

    results[[run]] <- run_result_list

  }

  return(results)
}
