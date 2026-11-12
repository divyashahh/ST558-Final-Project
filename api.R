library(plumber)
library(ranger)

set.seed(558)

# Read data
housing <- read.csv(
  "data/Melbourne_housing_FULL.csv",
  stringsAsFactors = FALSE
)

# Keep only variables used in the final model
housing_model <- housing[
  ,
  c(
    "Price",
    "Rooms",
    "Type",
    "Distance",
    "Bathroom",
    "Car",
    "Landsize",
    "Regionname"
  )
]

# Convert numeric variables
housing_model$Price <- as.numeric(housing_model$Price)
housing_model$Rooms <- as.numeric(housing_model$Rooms)
housing_model$Distance <- as.numeric(housing_model$Distance)
housing_model$Bathroom <- as.numeric(housing_model$Bathroom)
housing_model$Car <- as.numeric(housing_model$Car)
housing_model$Landsize <- as.numeric(housing_model$Landsize)

# Convert categorical variables
housing_model$Type <- factor(housing_model$Type)
housing_model$Regionname <- factor(housing_model$Regionname)

# Remove incomplete rows
housing_model <- housing_model[
  complete.cases(housing_model),
]

# Save factor levels for prediction
type_levels <- levels(housing_model$Type)
region_levels <- levels(housing_model$Regionname)

# Create dummy variables for training
x_train <- model.matrix(
  ~ Rooms + Type + Distance + Bathroom +
    Car + Landsize + Regionname,
  data = housing_model
)

# Remove intercept column
x_train <- x_train[, -1, drop = FALSE]

# Fit final random forest directly with ranger
final_model <- ranger(
  x = as.data.frame(x_train),
  y = housing_model$Price,
  mtry = 6,
  num.trees = 500,
  min.node.size = 5,
  importance = "impurity",
  seed = 558
)

# Function for finding the most common categorical value
get_mode <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

# Default values required for the API
default_rooms <- mean(housing_model$Rooms)
default_type <- get_mode(housing_model$Type)
default_distance <- mean(housing_model$Distance)
default_bathroom <- mean(housing_model$Bathroom)
default_car <- mean(housing_model$Car)
default_landsize <- mean(housing_model$Landsize)
default_region <- get_mode(housing_model$Regionname)


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
  
  # Convert incoming numeric values
  Rooms <- as.numeric(Rooms)
  Distance <- as.numeric(Distance)
  Bathroom <- as.numeric(Bathroom)
  Car <- as.numeric(Car)
  Landsize <- as.numeric(Landsize)
  
  # Check for invalid numeric values
  if (
    is.na(Rooms) ||
    is.na(Distance) ||
    is.na(Bathroom) ||
    is.na(Car) ||
    is.na(Landsize)
  ) {
    return(
      list(
        error = "Rooms, Distance, Bathroom, Car, and Landsize must be numeric."
      )
    )
  }
  
  # Check categorical inputs
  if (!(Type %in% type_levels)) {
    return(
      list(
        error = "Invalid Type. Use h, u, or t."
      )
    )
  }
  
  if (!(Regionname %in% region_levels)) {
    return(
      list(
        error = "Invalid Regionname."
      )
    )
  }
  
  # Create new observation
  new_property <- data.frame(
    Rooms = Rooms,
    Type = factor(
      Type,
      levels = type_levels
    ),
    Distance = Distance,
    Bathroom = Bathroom,
    Car = Car,
    Landsize = Landsize,
    Regionname = factor(
      Regionname,
      levels = region_levels
    )
  )
  
  # Create dummy variables using the same structure as training data
  new_x <- model.matrix(
    ~ Rooms + Type + Distance + Bathroom +
      Car + Landsize + Regionname,
    data = new_property
  )
  
  # Remove intercept
  new_x <- new_x[, -1, drop = FALSE]
  
  # Add any missing dummy-variable columns
  missing_cols <- setdiff(
    colnames(x_train),
    colnames(new_x)
  )
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      new_x <- cbind(
        new_x,
        setNames(
          data.frame(0),
          col
        )
      )
    }
  }
  
  # Keep columns in exactly the same order as training data
  new_x <- new_x[
    ,
    colnames(x_train),
    drop = FALSE
  ]
  
  # Generate prediction
  prediction <- predict(
    final_model,
    data = as.data.frame(new_x)
  )$predictions
  
  list(
    predicted_price = prediction
  )
}


#* Project information
#*
#* @get /info
function() {
  
  list(
    name = "Divya Shah",
    project_site =
      "https://divyashahh.github.io/ST558-Final-Project/modeling.html"
  )
}


# Example API calls:
#
# http://localhost:8000/pred
#
# http://localhost:8000/pred?Rooms=3&Type=h&Distance=10&Bathroom=2&Car=2&Landsize=500&Regionname=Northern%20Metropolitan
#
# http://localhost:8000/pred?Rooms=2&Type=u&Distance=5&Bathroom=1&Car=1&Landsize=100&Regionname=Southern%20Metropolitan