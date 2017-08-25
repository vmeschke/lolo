package io.citrine.lolo.transformers

import io.citrine.lolo.{Learner, Model, PredictionResult, TrainingResult}

/**
  * Manipulate the inputs of the model to create new polynomial equations
  * representing the input data.
  *
  * created by vmeschke on 8/24/17
  */
class PolynomialFan (baseLearner: Learner) extends Learner() {

  /**
    * Manipulate the x data in various nonlinear ways. Pass data through to learner.
    * Data will be transformed in the following ways:
    * All x's squared
    * exp(x)
    * More to come soon...    *
    *
    * @param trainingData Data to train on
    * @param weights      Weights of training rows if used
    * @return training result containing a model
    */
  override def train(trainingData: Seq[(Vector[Any], Any)], weights: Option[Seq[Double]]): StandardizerTrainingResult = {
    // Get the data in a form I can work with. Do this with a for loop to grab each row of a column
    //TODO actually figure out how to get the right piece of the array
    // May actually want ot remove categorical data as well
    var columns = Seq[(Vector[Any])]
    val anyRow = trainingData(0)
    for(col <- 0 to columns.length) {
      for (row <- 0 to anyRow.length) {
        columns(row)(col) = trainingData(0)
      }
    }

    val listOfOperations = Vector("Squared", "Exponential", "Other")
    for(i <- 0 to listOfOperations.length) {
      var op = listOfOperations(i)
      op match {
        case "Squared" =>
        case "Exponential" =>
        case _ =>
      }
    }
    return null
  }
}
