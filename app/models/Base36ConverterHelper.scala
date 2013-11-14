package models

// This object converts from and back to Base36 representation from Long
// The Base36 number represents the shortUrl code
object Base36ConverterHelper {

  // Public methods
  
  // Function : ConvertToBase64String
  // Arguments: 
  // x	: Long		-	The number to convert to Base36String
  // Returns  :
  // 	Base36 String representing the Long
  def ConvertToBase36String(number:Long) = ConvertToBase36TailRecursive(number, "")

  // Function : ConvertBase36StringToLong
  // Arguments: 
  // s	: String		-	The number (base36 format) to convert to Long
  // Returns  :
  // 	Long representing the Base36 String
  def ConvertBase36StringToLong(s: String): Option[Long] = {
	  	val zero:Option[Long] = Some(0.toLong)
    
		s.foldLeft(zero)(foldFunction)
  }

  private def foldFunction(num: Option[Long], digit: Char) = {
				num match {
			  		  case Some(number: Long) => {
					  		val c = ConvertBase36CharToLong(digit);
					  		c match {
					  		  	case None => None;
					  		  	case Some(l: Long) => Some(number * 36 + l)
					  		  	}
			  		  }
			  		  case _ => None
				} //match
	} //end of function

  
  //Private methods
  private def ConvertDigitToBase36Char(x: Long)  = {
		x match {
		  case it if 0 until 10 contains it  => ('0'.toInt + x).toChar
		  case it if 10 until 36 contains it => ('a'.toInt + (x - 10)).toChar
		  case _ => throw new Exception("Invalid digit " + x )
		}
	}
	
  private def ConvertToBase36TailRecursive( x:Long, soFar: String): String = {
  	x match {
  	  case n if (n < 0) => throw new Exception("-ve number " + x)
  		case 0            => soFar
  		case n if (n > 0) => {
					val add = ConvertDigitToBase36Char(x % 36) + soFar
					ConvertToBase36TailRecursive((x / 36).toInt, add)
  				}
  		}
  }

  private def ConvertBase36CharToLong(c: Char): Option[Long] = {
  	  	val lowerCase = c.toLower
  
  		lowerCase match {
  			case charInRange if ( (('0' until '9' + 1) contains charInRange) ||
  							(('a' until 'z' + 1) contains charInRange))
  							=> Some(ConvertBase36CharToLongInternal(lowerCase))
  							
  			case _ => None
  		}
  
  }
  
  private def ConvertBase36CharToLongInternal(c: Char): Long = {
  	  val lowerCase = c.toLower
  		lowerCase match {
  			case number if ('0' until '9' + 1) contains number => (lowerCase - '0')
  			case alphabet if ('a' until 'z' + 1) contains alphabet => (lowerCase - 'a') + 10
  			case _ => throw new Exception("Invalid character " + c )
  		}
  }
}