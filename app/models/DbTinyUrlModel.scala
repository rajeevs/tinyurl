package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

// DB model implementing the ModelBase interface but writing and reading to DB based on
// Base36 hashing scheme
object DbTinyUrlModel extends TinyUrlModelBase {
  
	def createUrlMapping(longUrl: String): Option[String] = {
	
		val insertedShortUrlCode = DB.withConnection { implicit c =>
		    SQL("insert into urlmappings (longurl, mappingalgoversion) values ({longurl}, {version})")
		    .on('longurl -> longUrl, 'version -> "1.0") 
		    .executeInsert()
		  }
		
		insertedShortUrlCode match {
		  case Some(shortUrlCode: Long) => {
		    val initClickStatsResult = initClickStats(shortUrlCode)
		    Some(Base36ConverterHelper.ConvertToBase36String(shortUrlCode))
		    
		  }
		  case None => None  
		}
	}

	private def initClickStats(shortUrlCode: Long)
	{
			val initQuery = SQL("insert into urlstats(shorturlid) values ({shorturlid})")
		    .on('shorturlid -> shortUrlCode) 

		    DB.withConnection { 
	  			implicit c => initQuery.execute()
			}
	}
	
	def getLongUrl(shortUrlCode: String): Option[String] = {
		val shortUrlIdResult = Base36ConverterHelper.ConvertBase36StringToLong(shortUrlCode)  
	    
		shortUrlIdResult match {
			  case None => None 
			  case Some(shortUrlId:Long)    => {
			
					val longUrlQuery =  SQL("select longurl from urlmappings where shorturlid = {shorturlid}").
									on('shorturlid-> shortUrlId)
			
					val stream = DB.withConnection { implicit c => longUrlQuery.apply()}
					
					val longUrl = GetHead(stream)
					
					longUrl match {
					  case None => longUrl
					  case _    => {    val success = updateClickStats(shortUrlId)
					    				longUrl
					  				}
					} //match
			  } //case Some
		} //match
	}
	
	def getStats(shortUrlCode: String): Option[Map[String, String]] = {

		val shortUrlIdResult = Base36ConverterHelper.ConvertBase36StringToLong(shortUrlCode)  

		shortUrlIdResult match {
			  case None => None
			  
			  case Some(shortUrlId:Long)    => {

				  	play.api.Logger.info("shortUrlIdResult is " + shortUrlId )

					val statsQuery =  SQL("select numclicks from urlstats where shorturlid = {shorturlid}").on('shorturlid-> shortUrlId)

					val results: List[Int] = DB.withConnection { 
				  	  	implicit c => statsQuery().map(
				  	  				row => row[Int]("numclicks")
				  	  			).toList
				  	}

				  	play.api.Logger.info("Found [" + results.size.toString + "] entries for shortUrlId =[ " + shortUrlId + " ]" )
				  	
					results match {
					  case List() => None
					  case _ => { Some(Map("numClicks" -> results.head.toString))}
					}
			  }
		}
	}
	
	private def updateClickStats(shortUrlId: Long) = {
		val updateClickStatsQuery =  SQL("update urlstats set numclicks = numclicks +1 where shorturlid = {shorturlid}").on('shorturlid-> shortUrlId)

		DB.withConnection { implicit c => updateClickStatsQuery.execute()}
	}
	
	
	private def GetHead(stream: Stream[anorm.SqlRow]):Option[String] = { 
		val firstRow = stream match { 
			case Stream() 	=> None; 
			case _ 		=> Some(stream.head) 
		} 
	
		firstRow match { 
				case Some(aRow:SqlRow)	=> 
				  				aRow match { 
									case Row(longurl:String) => Some(longurl) 
									case _ => None 
							   }
				case _ => None 
			}
	}

	
	private def GetHeadStats(stream: Stream[anorm.SqlRow]):Option[Long] = { 
		val firstRow = stream match { 
			case Stream() 	=> None; 
			case _ 		=> Some(stream.head) 
		} 
	
		firstRow match { 
				case Some(aRow:SqlRow)	=> 
				  				aRow match { 
									case Row(numClicks:Long) => Some(numClicks) 
									case _ => None 
							   }
				case _ => None 
			}
	}

	
}



