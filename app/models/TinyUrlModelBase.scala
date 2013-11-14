package models

import scala.collection.immutable.Map

// Forms the abstract base class for TinyUrl Model Implementations 
abstract class TinyUrlModelBase {
  
  def createUrlMapping(longUrl: String): Option[String]
  
  def getLongUrl(shortUrlCode: String): Option[String]
  
  def getStats(shortUrlCode: String): Option[ Map[String, String] ]
  
}

