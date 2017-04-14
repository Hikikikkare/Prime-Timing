package scalaTest



import scala.io._
import javax.swing._
import java.awt._
import java.awt.event._
import javax.imageio.ImageIO
import java.io.File
import java.awt.Image._

import scala.collection.mutable.ArrayBuffer

class Interface extends Display with helpful{

	var command = ""
			var talkflg=false
			val old_commands = new ArrayBuffer[String]

			def out(text: String, newline: Boolean = true){
		if(newline)
			printf("%s\n", text)
			else
				printf("%s", text)
	}

	def out(text: Int){
		out(text.toString(), false);
	}

	def updateDescription(loc: Int){
		val text = Database.query("kuvaus", "location", "idlocation = "+loc)
				if(text.length < 1){
					out("Error:: cannot find place description")
				}else{
					out(text(0))
				}
	}

	def read = {
			var str=""
					if(command!=""){
						str=get_command
					}
			str
	}



	

			def update(location : String){
		draw(location + ".png")
		draw_on_top("rulla.png", %%(rulla_location._1,rulla_location._2), %%(rulla_size._1,rulla_size._2))
	}
	/*
	 * Creates resizable Layout for the game
	 * image area = 5/6 height and textfield = 1/6
	 */
	frame.addKeyListener(new KeyListener(){ 
		def keyTyped(e:KeyEvent){
			val typed = e.getKeyChar :Int
					if(e.getKeyCode== KeyEvent.VK_ENTER || e.getKeyChar== '\n'){
					  println("command!" + command)
						command=panel.written_letter
								panel.written_letter=""

					}else{
						draw_text(e.getKeyChar)
					}

		}
		def keyPressed(e:KeyEvent){}
		def keyReleased(e:KeyEvent){}})

	def print_old_commands(){
		if(old_commands.length > 4){
			old_commands.remove(0)
		}
		for(com <- old_commands.reverse){
			draw_string(com,%%(laatikko_location._1 + 50, laatikko_location._2),30,Color.WHITE)
		}
	}

	def npc_shoutout(NPC : Npc){
		if(NPC != null){
			draw_string(NPC.name + " is here!", (50,550), 30, Color.WHITE)
		}
	}
	def is_ok_to_draw_string(): Boolean ={
			val ret = panel.clear_flag
					ret
	}
	def i_set_clear_flag(b : Boolean){
		panel.set_clear_flag(b)
	}

	//def clear_strings(){panel.strings.clear}
	/*
	 * returns command and sets the previous command to ""
	 */
	def get_command()={
			val ret = command
					if(command != ""){
						println("command in get_command ====>" + command)
						old_commands.append(ret)
					}
			command=""
					ret
	}

	def talkmode(npc : Npc, location_image : String){
		draw(location_image)
		draw_on_top(npc.image, %%(0,0),%%(30,35))
		draw_on_top("dialogi.png", %%(rulla_location), %%(rulla_size))
		var answer = false
		draw_string(npc.speak(),(600,130),30)
		panel.autoclear=false
		while(command != "exit"){

			/*
			 * do this if(command != "") better if you have time
			 */
			if(command != ""){
				println("COMMAND!!!! ========> " + command)
				panel.strings.clear()
				draw_string(npc.speak(),(600,130),30)
				var ans = npc.listen(command)
				draw_on_top("speech_bubble.png", %%(speech_bub_location),%%(speech_bub_size))
				draw_string(npc.name + " : " + ans,%%(talkmode_answer_location),25,Color.BLACK)
				command = ""
			}
			if(answer){

			}

			Thread.sleep(10)
			//panel.strings.clear()

		}
		panel.autoclear=true
				update(location_image)
				println("bye!!!")
	}

}




