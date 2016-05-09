library( caret )

# make NAs consistent
exercise_test <- read.csv( "data/pml-testing.csv", na.strings = c( "", "NA", "NULL", "#DIV/0!" ) )
exercise_test_raw <- read.csv( "data/pml-testing.csv", na.strings = c( "", "NA", "NULL", "#DIV/0!" ) )
exercise_train <- read.csv( "data/pml-training.csv", na.strings = c( "", "NA", "NULL", "#DIV/0!" ) )

#print( names( exercise_test_raw ) )
#print( str( names( exercise_test_raw ) ) )

# test set doesn't contain target column 'classe'
print( "classe" %in% names( exercise_test_raw ) )

head( exercise_test[ , 1:8 ] )
head( exercise_train[ , 1:8 ] )
print( dim( exercise_test ) )

# by collecting column names from test set, we'll make sure that we're using same columns in both sets
# is.na( exercise_test ) provides cellxcell calc of NA presence
# colSums essentially tallies up the "1"s reprersenting NAs.
# only include column name in feature name if sum of NAs in a dgiven column is == 0
# ommiting columns 1-7: X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp new_window
features <- names( exercise_test[ , colSums( is.na( exercise_test ) ) == 0 ] )[ 8:59 ]
length( features )
#append( features, "classe" )
#length( features )

exercise_train <- exercise_train[ , c( features, "classe" ) ]
exercise_test  <- exercise_test [ , c( features, "problem_id" ) ]

dim( exercise_train ); 
dim( exercise_test );

# 100% accuracy.  WUH?!?
predict_rf <- function() {
    
    # create model
    model_rf <- randomForest( classe ~ ., data = exercise_train, do.trace = TRUE, ntrees = 10 )
    summary( model_rf )
    
    # training
    predictions_rf_train <- predict( model_rf, newdata = exercise_train, type = "class" )
    summary( exercise_train )
    print( confusionMatrix( exercise_train$classe, predictions_rf_train )$table )
    print( confusionMatrix( exercise_train$classe, predictions_rf_train )$overall[ 'Accuracy' ] )
    
    # test
    predictions_rf_test <- predict( model_rf, newdata = exercise_test, type = "class" )
    print( predictions_rf_test )
    
    # not much to do analysis-wise here, 'classe' column is absent in test set
}

# 50% accuracy?!?
predict_rpart <- function() {

    #model_all <- glm( classe ~., data = exercise_train, family = "binomial" )
    model_all <- train( classe ~., data = exercise_train, method = "rpart" )
    summary( model_all )$coef
    
    # DON'T DO THIS! BLOWS UP R!?!
    #library( rattle )
    #fancyRpartPlot( model_all$finalModel ) 
    
    #model_auto <- step( model_all, trace = "TRUE" )
    #dim( summary( model_auto )$coef )
    #summary( model_auto )$coef
    #summary( model_auto )
    
    
    # get predictions
    predictions_train <- predict( model_all, newdata = exercise_train )
    predictions_test <- predict( model_all, newdata = exercise_test )
    str( predictions_train )
    summary( predictions_train )
    head( predictions_train )
    
    #calculate rmse?
    library( caret )
    print( confusionMatrix( exercise_train$classe, predictions_train )$table )
    print( confusionMatrix( exercise_train$classe, predictions_train )$overall[ 'Accuracy' ] )
}
predict_rf()
#predict_rpart()
