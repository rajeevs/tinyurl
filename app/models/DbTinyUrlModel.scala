/* 
 * Author: Rajeev Sudhakar
 * Date  : 11/9/2013
 */

package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

/* DB model implementing the ModelBase interface but writing and reading to DB based on
* Base36 hashing scheme
*/
object DbTinyUrlModel extends TinyUrlModelBase {
  
    /* Returns tinyUrl for long Url and adds mapping to DB. Code starts at 1 and is [0-9a-z] with max of 6 chars
     * 
     *  Detailed:
     *  Adds an entry to urlmappings table for original long url. id field is automatically incremented and we 
     *  convert this to Base36 Encoding [0-9a-z] and return Base36 string. One reason for this is that Base36 with
     *  only lowercase (26 chars + 10 digits = 36) is readable and makes sure case isn't an issue and can be easily copied 
     *  around. Base64 with UPPER/LOWERCASE is also an option
     *  
     *  The urlstats table is also initialized with 0 clicks for this shorturlid. 
     *  
     *  
     *  @returns :
     *         Some(shortUrl Code) 
     *         None on any failure
     *         
     *  TODO: 
     *  	- For optimization, we should do both DB operations in a separate DB call
     *      - Similarly, the urlStats are likely to be updated frequently, so ideally should be cached using memcached
     *      - The URL mapping table also is ideal for mongo DB type NoSQL storage since there is no relational DB work done here (transactions / Joins etc)
     *  
     */
	def createUrlMapping(longUrl: String): Option[String] = {
	
		val insertedShortUrlCode = DB.withConnection { 
		  implicit c =>
			    SQL("insert into urlmappings (longurl, mappingalgoversion) values ({longurl}, {version})")
			    .on('longurl -> longUrl, 'version -> "1.0") 
			    .executeInsert()
		  }
		
		insertedShortUrlCode match {
			  case Some(shortUrlCode: Long) => { 
				  									val initClickStatsResult = initClickStats(shortUrlCode)
				  									Some(Base36ConverterHelper.ConvertToBase36String(shortUrlCode))
			  									}
			  case _ => None  
		}
	}

	/* 
	 * Initializes urlstats entry for shortUrlCode
	 * 
	 * @returns boolean success of operation
	 */
	private def initClickStats(shortUrlCode: Long)
	{
		val initQuery = SQL("insert into urlstats(shorturlid) values ({shorturlid})").on('shorturlid -> shortUrlCode) 
		
		DB.withConnection { implicit c => initQuery.execute() }
	}
	
	/*
	 * Returns original long url for shortUrl code
	 * 
	 * @returns 
	 * 	None 			if mapping not foound for shortUrlCode
	 *  Original url 	if mapping found
	 */
	def getLongUrl(shortUrlCode: String): Option[String] = {
		val shortUrlIdResult = Base36ConverterHelper.ConvertBase36StringToLong(shortUrlCode)  
	    
		shortUrlIdResult match {
			  case None 					=> None 
			  case Some(shortUrlId:Long)    => {
			
					val longUrlQuery =  SQL("select longurl from urlmappings where shorturlid = {shorturlid}").
									on('shorturlid-> shortUrlId)
			
					val stream = DB.withConnection { implicit c => longUrlQuery.apply()}
					
					val longUrl = GetHead(stream)
					
					longUrl match {
						  case None => 		longUrl
						  case _    => 	{    val success = updateClickStats(shortUrlId); longUrl	}
					} //match
			  } //case Some
		} //match
	}

	/* Returns click statistics for shorturl 
	 * 
	 * @returns 
	 *  None if no mapping found
	 *  Map["numclicks" -> "# of clicks"] if mapping found
	 */
	def getStats(shortUrlCode: String): Option[Map[String, String]] = {

		val shortUrlIdResult = Base36ConverterHelper.ConvertBase36StringToLong(shortUrlCode)  

		shortUrlIdResult match {
			  case Some(shortUrlId:Long)    => {
								  	play.api.Logger.info("shortUrlIdResult is " + shortUrlId )
				
									val statsQuery =  SQL("select numclicks from urlstats where shorturlid = {shorturlid}").on('shorturlid-> shortUrlId)
				
									val results: List[Int] = DB.withConnection { 
											implicit c => statsQuery().map(row => row[Int]("numclicks")).toList
								  	}
				
								  	play.api.Logger.info("Found [" + results.size.toString + "] entries for shortUrlId =[ " + shortUrlId + " ]" )
								  	
									results match {
										  case List() 	=> None
										  case _ 		=> { Some(Map("numClicks" -> results.head.toString))}
									}
							  }
			  case _ => None
		}
	}
	
	/*
	 * Increments # of clicks by 1 for given shorturl 
	 */
	private def updateClickStats(shortUrlId: Long) = {
		val updateClickStatsQuery =  SQL("update urlstats set numclicks = numclicks +1 where shorturlid = {shorturlid}").on('shorturlid-> shortUrlId)

		DB.withConnection { implicit c => updateClickStatsQuery.execute()}
	}
	
	/* 
	 * Gets 1st longurl from DB stream of results
	 */
	private def GetHead(stream: Stream[anorm.SqlRow]):Option[String] = { 
		val firstRow = stream match { 
										case Stream() 	=> None; 
										case _ 			=> Some(stream.head) 
									 } 
	
		firstRow match { 
				case Some(aRow:SqlRow)	=> aRow match {  
												case Row(longurl:String) => Some(longurl) 
												case _ => None 
											}
				case _ 					=> None 
			}
	}
} //End of Object
