
read_prj_config <- function() {
    prj_config_path <- here::here("_project.yml")
    if (!file.exists(prj_config_path)) {
        return(NULL)
    }
    config <- yaml::read_yaml(prj_config_path)
    return(config)
}