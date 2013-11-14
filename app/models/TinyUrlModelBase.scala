/* 
 * Author: Rajeev Sudhakar
 * Date  : 11/9/2013
 */

package models

import scala.collection.immutable.Map

/*
 * Abstract base calss for TinyUrl models. Reference this only in constructors
 */ 
abstract class TinyUrlModelBase {
  
  def createUrlMapping(longUrl: String): Option[String]
  
  def getLongUrl(shortUrlCode: String): Option[String]
  
  def getStats(shortUrlCode: String): Option[ Map[String, String] ]
  
}

