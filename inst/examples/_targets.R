library(targets)
library(tarchetypes)

tar_option_set(
    packages = c("dplyr", "ggplot2", "broom"),
    format = "rds"
)

list(
    tar_target(
        raw_data,
        {
            set.seed(123)
            data.frame(
                site = rep(LETTERS[1:5], each = 20),
                rainfall = rnorm(100, 500, 100),
                temp = rnorm(100, 20, 5),
                yield = rnorm(100, 4, 1)
            )
        },
        description = "Simulated raw field experiment dataset with site, rainfall, temperature and yield"
    ),
    tar_target(
        clean_data,
        raw_data %>%
            mutate(
                rainfall = ifelse(rainfall < 0, NA, rainfall),
                temp = ifelse(temp < -10 | temp > 50, NA, temp)
            ) %>%
            tidyr::drop_na(),
        description = "Data cleaning step removing impossible values and missing observations"
    ),
    tar_target(
        site_summary,
        clean_data %>%
            group_by(site) %>%
            summarise(
                mean_yield = mean(yield),
                mean_rain = mean(rainfall),
                mean_temp = mean(temp)
            ),
        description = "Site-level summary statistics used for exploratory analysis"
    ),
    tar_target(
        exploratory_plot,
        ggplot(clean_data, aes(rainfall, yield)) +
            geom_point() +
            geom_smooth(method = "lm") +
            theme_minimal(),
        description = "Exploratory scatter plot showing rainfall–yield relationship"
    ),
    tar_target(
        model_formula,
        yield ~ rainfall + temp,
        description = "Linear regression model specification for yield prediction"
    ),
    tar_target(
        model_fit,
        lm(model_formula, data = clean_data),
        description = "Fitted linear regression model estimating climate effects on yield"
    ),
    tar_target(
        model_coefficients,
        broom::tidy(model_fit),
        description = "Extracted regression coefficients with standard errors and p-values"
    ),
    tar_target(
        model_predictions,
        clean_data %>%
            mutate(predicted_yield = predict(model_fit, newdata = clean_data)),
        description = "Model predictions of yield for each observation"
    ),
    tar_target(
        model_performance,
        {
            rmse <- sqrt(mean((model_predictions$yield - model_predictions$predicted_yield)^2))
            r2 <- cor(model_predictions$yield, model_predictions$predicted_yield)^2
            data.frame(rmse = rmse, r2 = r2)
        },
        description = "Model evaluation metrics including RMSE and R-squared"
    ),
    tar_target(
        performance_plot,
        ggplot(model_predictions, aes(predicted_yield, yield)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
            theme_minimal(),
        description = "Predicted vs observed yield plot to evaluate model performance"
    ),
    tar_target(
        report_summary,
        list(
            coefficients = model_coefficients,
            performance = model_performance
        ),
        description = "Final analysis summary combining model coefficients and performance metrics"
    )
)
