library(plumber)
library(tidyverse)
library(tidymodels)
library(ranger)

tidymodels_prefer()

# Read and prepare data
housing <- read_csv(
  "data/Melbourne_housing_FULL.csv",
  show_col_types = FALSE
)

housing_model <- housing |>
  select(
    Price,
    Rooms,
    Type,
    Distance,
    Bathroom,
    Car,
    Landsize,
    Regionname
  ) |>
  drop_na() |>
  mutate(
    Type = factor(Type),
    Regionname = factor(Regionname)
  )

# Helper function for most common category
get_mode <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

# Default predictor values
default_rooms <- mean(housing_model$Rooms)
default_type <- get_mode(housing_model$Type)
default_distance <- mean(housing_model$Distance)
default_bathroom <- mean(housing_model$Bathroom)
default_car <- mean(housing_model$Car)
default_landsize <- mean(housing_model$Landsize)
default_region <- get_mode(housing_model$Regionname)

# Recipe used by final model
housing_recipe <- recipe(
  Price ~ .,
  data = housing_model
) |>
  step_dummy(all_nominal_predictors())

# Final random forest model
rf_spec <- rand_forest(
  mtry = 6,
  trees = 500,
  min_n = 5
) |>
  set_engine(
    "ranger",
    importance = "impurity"
  ) |>
  set_mode("regression")

final_workflow <- workflow() |>
  add_recipe(housing_recipe) |>
  add_model(rf_spec)

# Fit final model to full dataset
final_model <- final_workflow |>
  fit(data = housing_model)


#* Predict Melbourne housing price
#*
#* @param Rooms Number of rooms
#* @param Type Property type: h, u, or t
#* @param Distance Distance from Melbourne CBD
#* @param Bathroom Number of bathrooms
#* @param Car Number of car spaces
#* @param Landsize Property land size
#* @param Regionname Geographic region
#* @get /pred
function(
    Rooms = default_rooms,
    Type = default_type,
    Distance = default_distance,
    Bathroom = default_bathroom,
    Car = default_car,
    Landsize = default_landsize,
    Regionname = default_region
) {
  
  new_property <- tibble(
    Rooms = as.numeric(Rooms),
    Type = factor(
      Type,
      levels = levels(housing_model$Type)
    ),
    Distance = as.numeric(Distance),
    Bathroom = as.numeric(Bathroom),
    Car = as.numeric(Car),
    Landsize = as.numeric(Landsize),
    Regionname = factor(
      Regionname,
      levels = levels(housing_model$Regionname)
    )
  )
  
  if (is.na(new_property$Type)) {
    return(list(
      error = "Invalid property Type. Use h, u, or t."
    ))
  }
  
  if (is.na(new_property$Regionname)) {
    return(list(
      error = "Invalid Regionname."
    ))
  }
  
  prediction <- predict(
    final_model,
    new_data = new_property
  )
  
  list(
    predicted_price = prediction$.pred
  )
}


#* Project information
#*
#* @get /info
function() {
  
  list(
    name = "Divya Shah",
    project_site = "https://divyashahh.github.io/ST558-Final-Project/modeling.html"
  )
}


# Example API calls:
#
# http://localhost:8000/pred
#
# http://localhost:8000/pred?Rooms=3&Type=h&Distance=10&Bathroom=2&Car=2&Landsize=500&Regionname=Northern%20Metropolitan
#
# http://localhost:8000/pred?Rooms=2&Type=u&Distance=5&Bathroom=1&Car=1&Landsize=100&Regionname=Southern%20Metropolitan