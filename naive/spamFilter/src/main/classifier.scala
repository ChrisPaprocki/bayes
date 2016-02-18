package bayes.naive.spamFilter


/** A classifier used to judge if the text is spam or ham.
  *
  * @constructor create  a new classifier with threshold and searched words
  * @param threshold the threshold above which the text is considered as a spam
  * @param wnProbs   words and its probability to occur in spam and ham
  */
class Classifier(threshold: Double,
                 wnProbs: Map[String, (Double, Double)]) {

  /** Checks the presence of the wn words in given text
    *
    * @param text the text to be checked
    * @param wn   words to be searched in given text
    * @param text the text to be checked
    * @return wn presence
    */
  private def checkPresence(text: Array[String], wn: Array[String]) = {
    wn.foldLeft(Map.empty[String, Boolean]) { (presence, word) => presence + (word -> text.contains(word)) }
  }


  /** Calculates the probability of the text to be a spam
    *
    * @param text the text to be assessed
    * @return the probability
    */
  def calcProbOfSpam(text: Array[String]) = {
    val presence = checkPresence(text, wnProbs.keySet.toArray)

    // TODO: Make it 'foldish', without vars
    var resSpam: Double = 1.0
    var resHam: Double  = 1.0
    for(x <- presence) {
      if (x._2 == true) {
        resSpam = resSpam * wnProbs(x._1)._1 // get the spam probability, safe: no way to have None
        resHam = resHam * wnProbs(x._1)._2   // get ham probability
      }
      else {
        resSpam = resSpam * (1 - wnProbs(x._1)._1) // get the spam probability
        resHam = resHam * (1 - wnProbs(x._1)._2)   // get ham probability
      }
    }

    resSpam / (resSpam + resHam)
  }

  /** Returns true if spam, false if ham
    *
    * @param text the text to be classified
    */
  def classify(text: Array[String]) = {
    calcProbOfSpam(text) > threshold
  }
}
