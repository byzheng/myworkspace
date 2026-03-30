
read_prj_config <- function() {
    prj_config_path <- path_prj("_project.yml")
    if (!file.exists(prj_config_path)) {
        return(NULL)
    }
    config <- yaml::read_yaml(prj_config_path)
    return(config)
}

#' Get a project configuration value
#' 
#' Retrieves a specific configuration value from the project's `_project.yml` file.
#' Supports nested keys using dot notation (e.g., "start.year").
#' 
#' @param name The name of the configuration value to retrieve. Can be a nested key using dot notation.
#' @return The value of the specified configuration key
get_prj_config <- function(name) {
    config <- read_prj_config()
    if (is.null(config)) {
        stop("No project configuration found. Please create a _project.yml file.")
    }
    optree_config <- optree::create_options_manager(defaults = config)
    value <- optree_config$get(name)
    return(value)
}
