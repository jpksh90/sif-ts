import Op.{Div, Minus, Mul, Plus}

import scala.util.parsing.combinator.*


class CommandParser(input: String) {



}



class Tokenizer {
  sealed trait Token

  case class Identifier(name: String) extends Token

  case class Number(value: Int) extends Token

  case class Operator(symbol: Char) extends Token

  case object Unknown extends Token


  def tokenize(input: String): List[String] = {
    val tokens = collection.mutable.ListBuffer[String]()
    var currentToken: String = ""
    var currentpos: Int = 0;
    while (currentpos < input.length) {
      val char = input.charAt(currentpos);

      if (currentpos == input.length-1) {
        // end of stream
        currentToken += char
        tokens.addOne(currentToken);
      } else {
        char match
          case c if c.isDigit => currentToken += c
          case c if c.isLetter => currentToken += c
          case c if c == '=' => {
            currentToken += c;
            currentpos = currentpos+1
            val nextToken = input.charAt(currentpos);
            if (nextToken == '=') {
              // check for == tokens
              currentToken = currentToken+nextToken;
              tokens.addOne(currentToken);
              currentToken = ""
            } else {
              currentpos = currentpos - 1;
            }
          }

          case c if c == '>' => {
            currentToken += c;
            currentpos = currentpos + 1
            val nextToken = input.charAt(currentpos);
            if (nextToken == '=') {
              // check for == tokens
              currentToken = currentToken + nextToken;
              tokens.addOne(currentToken);
              currentToken = ""
            } else {
              currentpos = currentpos - 1;
            }
          }
          case _ => {
            tokens.addOne(currentToken)
            currentToken = ""
          }
      }
      currentpos = currentpos + 1;
    }
    tokens.toList
  }

  private def createToken(tokenString: String): Token = {
    if (tokenString.matches("""\d+""")) {
      Number(tokenString.toInt)
    } else {
      Identifier(tokenString)
    }
  }
}

object CommandParser {
  def main(args: Array[String]) = {
    println(Tokenizer().tokenize("x9=a1+12"));
  }
}