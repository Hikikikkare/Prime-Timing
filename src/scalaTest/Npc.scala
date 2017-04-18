package scalaTest

import scala.collection.mutable.ArrayBuffer



class Npc(val name: String, val location: Int, val image : String = "kuva1.png") {
  
  val items = new ArrayBuffer[String]
  
  
  def addItem(item: String){
    items.append(item)
  }
  
  def checkItem(item: String)={
    val aa = for(s <- items if s == item) yield 0
    if (aa.length > 0) true else false
  }
  
  def removeItem(item: String){
    items -= item
  }
  
  
  
  def alert(){
		println(name + " is here!") 
	}
  
	def speak()={
			var ret = ""
					var speak = Database.query("*","characters", "name = \"" + name + "\"")
					speak.remove(17)// remove id location
					speak.remove(1)// remove id speak
					for(s <- speak){
						ret = ret + s + '\n'
					}
			ret
	}
	
	def listen(player_answer : String)={
			var ans = ""
					if(player_answer.length == 1 && player_answer.last < 107 && player_answer.last > 96){
						var speak = Database.query("answer","answers", "name = \"" + name + "\" and abc = \"" + player_answer + "\"")
								if(speak.length > 0){
									ans = speak(0)
											//tänne jos haluu jotain tehtävää updatee puhumalla jollekki
								}else{ans = "say something else!"}
					}else{ans = "say something else!"}
			ans
	}
	
	var quest_mode = 0

			def set_questmode(new_quest_mode : Int){
		quest_mode = new_quest_mode
	}
	override def toString : String = {
	  name
	}
  
  
}

object Npc{
  
  def apply(name: String, location: Int=0, img: String)={
    var loc = location;
    if(location == 0 ){
      val list = Database.query("idlocation", "characters")
      if(list.length > 0){
        loc = list(0).toInt
      }
    }
    new Npc(name, loc, img)
  }
  
  
}


