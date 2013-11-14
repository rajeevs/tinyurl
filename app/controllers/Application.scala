/* 
 * Author: Rajeev Sudhakar
 * Date  : 11/9/2013
 */

package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import models._
import java.net.URI
import views.html.defaultpages.notFound
import play.api.libs.iteratee.Enumerator
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {

  private var modelImpl: TinyUrlModelBase = DbTinyUrlModel;
  private var defaultUriSchemePrefix = "http://"
  
  val createUrlForm = Form( "longurl" -> nonEmptyText )
    
  def index = Action {
    Ok(views.html.index(createUrlForm))
  }

  /* Function createtinyurl 
   * Synopsis: Returns a tiny url for given url as Json
   * 
   * Details: 
   * - REST entrypoint
   * - Adds an url mapping for long url -> shortened Url in model and returns Json
   * 
   * @returns: 
   *  HTTP 200: success. returns Json object with fields [ "originalurl", "tinyurl", and "statslink" ]
   *  HTTP 400: form input doesn't contain longurl field
   *  HTTP 500: any other failure
   */
  def createtinyurl = Action {
	  implicit Request => {
	    
	    // TODO: add checks for 
	    //		1. forbidden protocols 
	    //		2. forbidden forwarding domains/urls as regexs
	    //		3. DOS / SPAM / BOT detection logic based on 
	    //				a. incoming ip addresses 
	    //				b. # of request / last hr   
		 
				  
		  createUrlForm.bindFromRequest.fold(
			    errors => BadRequest("Missing longUrl in form input"),
			    longUrl => {
			    				val correctedUrl = correctUrl(longUrl)
			    				
			    				val result = createtinyurlInternal(correctedUrl, Request.host)
			    				
			    				result match {
			    				  	case None => Status(500)("Unable to create short url")  
			    				  	case Some(tinyUrl:String) =>
			    				  	  {
			    				  		  	val resultsMap: Map[String, String] = Map("originalurl"-> correctedUrl,
			    				  		  						 "tinyurl" -> tinyUrl,
			    				  		  						 "statsLink" -> (tinyUrl + "/stats")
			    				  		  					)
			    				  		  	Ok(Json.toJson(resultsMap))
			    				  	  }
			    				}
			               }
			)
	  }
  }


  /* Returns http 303 redirect to original url for shorturl
   *
   * @returns 
   * 	HTTP 303 for original url
   *    HTTP 404 if the Url is not found
   */
  def redirect(shortUrlCode: String) = Action {

    play.api.Logger.info("redirect method called for [" + shortUrlCode + "]")

    val longUrl = modelImpl.getLongUrl(shortUrlCode)
    
    longUrl match { 
      case Some(actualLongUrl: String) => Redirect(actualLongUrl)
      case _ => NotFound("This Url is invalid") 
    }
  }

  /* Returns number of clicks for the Url as Json object
   * 
   * @returns Json object {"numClicks":"0"}
   */
  def getstats(shortUrlCode: String) = Action {

    // Possible errors are invalid hashcode and too many errors 

    play.api.Logger.info("getstats method called for [" + shortUrlCode + "]")

    val stats = modelImpl.getStats(shortUrlCode)

    stats match {
      case Some(m:Map[String, String])   => Ok(Json.toJson(m)) 
      case _	                         => NotFound(shortUrlCode + " is not valid")
    }
  }
  
  /* Adds http:// prefix to Url if absent. Redirection won't work without URI scheme
   * @returns corrected url. If URI scheme is present, it won't be changed
   */
   private def correctUrl(originalUrl:String) = {
	    // If there isn't a protocol, add a http:// at the beginning
	    val uri = URI.create(originalUrl)
	    
	    uri.getScheme match {
	      						case null 	=> defaultUriSchemePrefix + originalUrl
	      						case _		=> originalUrl 
	    }
  }
  
   /* Returns a tinyUrl
    * 
    * @returns : Option[String]
    * 	Some(shortened Url) if success
    *   None if there is an error
    */
  private def createtinyurlInternal(longUrl: String, hostUri : String) = {
    
    play.api.Logger.info("createtinyurl method called for [" + longUrl + "]")

    // call model to add URL and provide Url, domain name
    // Model returns the full tiny url

    // NOTE: Not using a view to send Json because only text and html templates
    // are supported by Play and not Json templates. We would have to set the Json template as 
    // text template and override the content-type to Json which is hacky. 
    // Plus the view part in JSON results is fairly minimal, so it doesn't require a new view. 
    
    val shortUrlCode = modelImpl.createUrlMapping(longUrl);
    
    shortUrlCode match {
      case Some(shortUrl:String)    => {
            val tinyUrl = (defaultUriSchemePrefix + hostUri  + "/" + shortUrl);

		    play.api.Logger.info("createtinyurl returning tinyurl [" + tinyUrl + " ] for original Url " + longUrl + " ]")
		
		    Some(tinyUrl)
      	}
      case _ => None
    }
  }
}