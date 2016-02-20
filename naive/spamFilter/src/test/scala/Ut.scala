package bayes.naive.spamFilter

import org.scalatest._

/** A base class for all unit tests.
  *
  */
abstract class Ut extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors
  
