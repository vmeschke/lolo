package io.citrine.lolo.linear

import breeze.linalg.{DenseMatrix, DenseVector, diag, pinv, sum}
import io.citrine.lolo.{Learner, Model, PredictionResult, TrainingResult}

/**
  * Lasso  regression learner
  *
  * Kind of written by vmeschke
  *
  * @param fitIntercept whether to fit an intercept or not
  */
class LassoRegressionLearner(fitIntercept: Boolean = true) extends Learner {

  var hypers: Map[String, Any] = Map("regParam" -> 0.0)

  /**
    * Train a lasso model via direct inversion.
    *
    * @param trainingData to train on
    * @param weights      for the training rows, if applicable
    * @return a model
    */
  override def train(trainingData: Seq[(Vector[Any], Any)], weights: Option[Seq[Double]]): LassoRegressionTrainingResult = {
    //TODO ask Max about which pieces of training data are which and how to access them
    val n = trainingData.size
    val xData = trainingData(0)
    val yActual = trainingData(0)
    val yPredicted = Vector()
    // This array keeps track of if a coefficient has been updated from 0
    var updatedFromZero = Vector(n){false}
    //TODO what should the coefficient step be?
    var coefStepSize = 0.1

    // Solve the LASSO problem using the Least Angle Regression approach
    // Set all regression coefficients to 0
    var coefs = Array.fill(n){0}
    var addedCoefs = Vector()

    // Determine which predictor (x) is most correlated with the output (y)
    //TODO would a helper method be useful here? And how do I get an x?
    var mostCorrelatedX = calcCorrelation(xData, yActual)
    // Calculate the residuals r (r = y - predicted_y)
    var r = residuals(yPredicted, yActual)

    // Find the correlation of all x's with the residuals
    var allXCorrsWithR = Vector()
    for(i <- 0 to xData(0).length) {
      allXCorrsWithR(i) = calcCorrelation(xData, r)
    }

    // While all have not been added to the regression equation:
    while(addedCoefs.length <= n) {
      // Determine which predictor (x) is most correlated with the output (y)
      // Do this with a for loop? Loop through all x's, calculate correlation, see which
      // is largest. Right now I have this returning
      // Bad: mostCorrelatedX = calcCorrelation(xData, yActual)

      // Determine level of correlation this predictor has with r
      var bestCorrWithR = calcCorrelation(, r)
      // Until another predictor has as much correlation with the residuals as this predictor:
      while(allXCorrsWithR.max() <= bestCorrWithR){
        // Increase the coefficient of most correlated predictor in the direction of the sign of its correlation with y
        allXCorrsWithR(INDEX) = allXCorrsWithR(INDEX) + coefStepSize
        updatedFromZero(INDEX) = true

        // Calculate residuals r
        r = residuals(yPredicted, yActual)
        // Check which predictor has the most correlation with r

        // Increase the coefficients for the 2 most correlated predictors in their joint least squares direction
        // until some other predictor x has as much correlation with the residual r.
        // If a coefficient was not zero before and hits 0, remove it from the list of predictors
        for(i <- 0 to updatedFromZero.length) {
          if(coefs(i) == 0 && updatedFromZero(i))

        }
      }

    }
    new LassoRegressionTrainingResult(model, hypers)
  }

  /**
  * Method to compute correlation of given x variable with class
  *
  * @param xData the x data (feature)
  * @param yData the y data (label)
  */
  def calcCorrelation(xData: Array[Int], yData: Array[Int]): Int = {
    //TODO write this method
    return 0
  }

  /**
  * Method to compute residuals of given x variable with class
  *
  * @param xData the x data (feature)
  * @param yData the y data (label)
  */
  def residuals(yPredicted: Array[Int], yActual: Array[Int]): Double = {
    var residuals = 0
    for(i <- 0 to yPredicted.length) {
      residuals += (yPredicted[i] - yActual[i])
    }

    return residuals
  }
}

/**
  * Simple container around the model
  *
  * @param model contained
  */
@SerialVersionUID(999L)
class LassoRegressionTrainingResult(model: LassoRegressionModel, hypers: Map[String, Any]) extends TrainingResult {
  /**
    * Get the hyperparameters used to train this model
    *
    * @return hypers set for model
    */
  override def getHypers(): Map[String, Any] = hypers

  override def getModel(): LassoRegressionModel = model

  /**
    * Get a measure of the importance of the model features
    *
    * @return feature influences as an array of doubles
    */
  override def getFeatureImportance(): Option[Vector[Double]] = {
    val beta = model.getBeta().map(Math.abs)
    val renorm = 1.0 / beta.sum
    Some(beta.map(_ * renorm))
  }
}

/**
  * Linear regression model as a coefficient vector and intercept
  *
  * @param beta      coefficient vector
  * @param intercept intercept
  * @param indices   optional indices from which to extract real features
  */
@SerialVersionUID(1000L)
class LassoRegressionModel(
                             beta: DenseVector[Double],
                             intercept: Double,
                             indices: Option[(Vector[Int], Int)] = None
                           ) extends Model[LassoRegressionResult] {

  /**
    * Apply the model to a seq of inputs
    *
    * @param inputs to apply the model to
    * @return a predictionresult which includes, at least, the expected outputs
    */
  override def transform(inputs: Seq[Vector[Any]]): LassoRegressionResult = {
    val filteredInputs = indices.map{case (ind, size) => inputs.map(inp => ind.map(inp(_)))}.getOrElse(inputs).flatten.asInstanceOf[Seq[Double]]
    val inputMatrix = new DenseMatrix(filteredInputs.size / inputs.size, inputs.size,
      filteredInputs.toArray
    )
    val resultVector = beta.t * inputMatrix + intercept
    val result = resultVector.t.toArray.toSeq
    val grad = getBeta()
    new LassoRegressionResult(result, grad)
  }

  /**
    * Get the beta from the linear model \beta^T X = y
    * @return beta as a vector of double
    */
  def getBeta(): Vector[Double] = {
    indices.map { case (inds, size) =>
      val empty = DenseVector.zeros[Double](size)
      inds.zipWithIndex.foreach { case (j, i) => empty(j) = beta(i) }
      empty
    }.getOrElse(beta).toArray.toVector
  }
}

/**
  * Simple container around the result and coefficient array
  *
  * @param values computed from the model
  * @param grad   gradient vector, which are just the linear coefficients
  */
class LassoRegressionResult(values: Seq[Double], grad: Vector[Double]) extends PredictionResult[Double] {
  /**
    * Get the expected values for this prediction
    *
    * @return expected value of each prediction
    */
  override def getExpected(): Seq[Double] = values

  /**
    * Get the gradient, which is uniform
    *
    * @return a vector of doubles for each prediction
    */
  override def getGradient(): Option[Seq[Vector[Double]]] = Some(Seq.fill(values.size)(grad))
}
