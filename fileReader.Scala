import scala.io.Source
import scala.collection.mutable.Map
val  filename = "testFile1.py"

  
import java.util.ArrayList

object Example1 {
  var typeDec:Map[String,String] = Map() 
  def main(args: Array[String]): Unit = {
    // Getting type Declarations
   // var typeDec:Map[String,String] = Map() 
    var Evalstate = new ArrayList[String]();
    // get DataTypes
    for (lines <- Source.fromFile(filename).getLines) {
      if lines.contains("#") && lines.contains("TDEC") then 
        val line = lines.replace(" ","")
        val splitValues =  line.split("#")
        val statement = splitValues(0)
        val variable = statement.split("=")(0)
        val declaration = splitValues(1)
        if declaration.contains("H") then
            typeDec += (variable -> "H")
        else 
            typeDec += (variable -> "L")
      // Getting evaluation Statements
      if lines.contains("#") && lines.contains("TEVAL") then
        val line = lines.replace(" ","")
        Evalstate.add(line.split("#")(0))
        }
      for (ind <- 0 to Evalstate.size()-1)
      {
        println(getClass(Evalstate.get(ind)))
      }
    
    // print""ing the lis}
    //println(typeDec);
    //println(Evalstate)
}
    def getClass(inState:String): String = { 
      var str = inState.replace(" ","")
      //print(typeDec)
      if str.contains("=") then
        {var inp = str.split("=") 
        var leftSide = getClass(inp(0))
        var rightSide =  getClass(inp(1))
        if leftSide== "L" && rightSide == "H" then return "FLAW"
        else return "NOFLAW"  
        }
      else if str.contains("+") then {
          var inp = str.split("\\+")
          var last = "L"
          for(exp <- inp){
            var newVar = getClass(exp)
            if newVar=="H" then last = "H"
          }
          return last
        }
        else if str.contains("-") then {
          var inp = str.split("\\-")
          var last = "L"
          for(exp <- inp){
            var newVar = getClass(exp)
            if newVar=="H" then last = "H"
          }
          return last
        }
        else if str.contains("*") then {
          var inp = str.split("\\*")
          var last = "L"
          for(exp <- inp){
            var newVar = getClass(exp)
            if newVar=="H" then last = "H"
          }
          return last
        }

        else if str.contains("/") then {
          var inp = str.split("\\/")
          var last = "L"
          for(exp <- inp){
            var newVar = getClass(exp)
            if newVar=="H" then last = "H"
          }
          return last
        }
      
      else if (typeDec.contains(inState)) then  return typeDec(inState)
      else return "L"
       
  }
}
