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
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def createtinyurl = Action {
	  implicit Request => {
	    
		 play.api.Logger.info(Request.host)

		  val createUrlForm = Form(
				  "longurl" -> nonEmptyText
				  )
				  
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


  def redirect(shortUrlCode: String) = Action {

    play.api.Logger.info("redirect method called for [" + shortUrlCode + "]")

    val longUrl = modelImpl.getLongUrl(shortUrlCode)
    
    longUrl match { 
      case Some(actualLongUrl: String) => Redirect(actualLongUrl)
      case _ => NotFound("This Url is invalid") 
    }
  }

  def getstats(shortUrlCode: String) = Action {

    // Possible errors are invalid hashcode and too many errors 

    play.api.Logger.info("getstats method called for [" + shortUrlCode + "]")

    val stats = modelImpl.getStats(shortUrlCode)

    stats match {
      case Some(m:Map[String, String])   => Ok(Json.toJson(m)) 
      case _	                         => NotFound(shortUrlCode + " is not valid")
    }
  }
  
    private def correctUrl(originalUrl:String) = {
	    // If there isn't a protocol, add a http:// at the beginning
	    val uri = URI.create(originalUrl)
	    
	    uri.getScheme match {
	      						case null 	=> defaultUriSchemePrefix + originalUrl
	      						case _		=> originalUrl 
	    }
  }
  
  private def createtinyurlInternal(longUrl: String, hostUri : String) = {
    
    play.api.Logger.info("createtinyurl method called for [" + longUrl + "]")

    // call model to add URL and provide Url, domain name
    // Model returns the full tiny url

    // TODO: move all view stuff to Mvc app including what's in JsonHelper
    // TODO: add checks for 
    //		1. forbidden protocols 
    //		2. forbidden forwarding domains/urls as regexs
    //		3. DOS / SPAM / BOT detection logic based on 
    //				a. incoming ip addresses 
    //				b. # of request / last hr   
    // NOTE: Not using a view to send Json because only text and html templates
    // are supported by Play and not Json templates. We would have to set the Json template as 
    // text template and override the content-type to Json which is hacky. 
    // Plus the view part in JSON results is fairly minimal, so it doesn't require a 
    // new view. 
    
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