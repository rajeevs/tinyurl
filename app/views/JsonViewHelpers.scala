package views

import play.api._
import play.api.libs.json.Json

object JsonHelpers {
	def getTinyUrlSuccessResult(url: String, tinyUrl: String) = {

	  play.api.Logger.info("JsonHelpers called with [" + url + ", " + tinyUrl + " ]")
		
	  Json.obj("originalUrl" -> url,"tinyUrl" -> tinyUrl )
	}
}