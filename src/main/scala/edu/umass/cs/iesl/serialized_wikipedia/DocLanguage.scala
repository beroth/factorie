package edu.umass.cs.iesl.serialized_wikipedia

sealed trait DocLanguage {
  def isoString:String
}

object DocLanguage {
  def fromIsoString(iso:String) = iso match {
    case English.isoString => English
    case Spanish.isoString => Spanish
  }
  def isSupportedLanguage(iso:String): Boolean = {
    (iso == English.isoString) || (iso == Spanish.isoString)
  }
}

case object English extends DocLanguage {val isoString = "en"}
case object Spanish extends DocLanguage {val isoString = "es"}

