package scalaTest

import scala.collection.mutable.ArrayBuffer
import parser.Parser

class Program(interface: Interface) {

	val characterList = Database.query("name","speaks");

	val opposite = Map("south" -> "north", "west" -> "east", "north" -> "south" , "east" -> "west")

			val characters = for (a <- characterList) yield Npc(a,0,"")
			val openQuests=new ArrayBuffer[Quests]
					val closedQuests=new ArrayBuffer[Quests]

							var map = new MyMap()
							var player = new Player(this, interface, map)
							var parsero = new Parser()

							var quick:Boolean=false;

							for(i <- 0 until Quests.amount) closedQuests += Quests(this,i, "q"+(i+1))

									openQuests += closedQuests(0)



									interface.setup_GUI()
									interface.update(player.location)



									def update(command : String){
								if(command!=""){
									var list = parsero.parse(command, quick)

											if(quick) quickMode(list)
											else interpret(list)

								}
								//graafa update
								if(!interface.is_ok_to_draw_string){
									interface.npc_shoutout(getNpcLocation(player.locationID))
									interface.print_old_commands()
									interface.print_description()
									interface.print_notifications()
									interface.i_set_clear_flag(true)
								}
							}

							def quickMode(list : ArrayBuffer[parser.Command]){
								for (command <- list){
									command.verb.str match{
									case "back" => {
										interface.out("going back")
										quick=false
										command.targets += new parser.Word(opposite(player.prevMovement)) with parser.order
										player.action(player.move,command.targets)
									}
									case "unkn" => {
										interface.out("type 'b' to go back")
									}
									case "toom" => {
										interface.out("too many commands")
									}
									case  _ => interface.out("unknown quick command")
									}
								}
							}

							def interpret(list : ArrayBuffer[parser.Command]){
								if(list.length <1){
									interface.out("unknown command")
								}else

									for(command <- list){

										command.verb.str match{
										case "move" => {
											if(player.action(player.move,command.targets)){
												interface.update(player.location)
												if(map.isBlocked(player.locationID)){
													quick=true
															interface.out("type 'b' to go back")
												}
											}


										}

										case "eat" => {
											player.eat(command.targets)
										}
										case "speak" => {
											player.action(player.speak,command.targets)
										}
										case "kill" => {
											player.action(player.kill,command.targets)
										}
										case "take" => {
											player.action(player.pick,command.targets)
										}
										case "look" => {
											player.lookAround(command.targets)
										}
										case "drop" => {
											player.action(player.drop,command.targets)
										}
										case "give" => {
											player.action(player.give,command.targets)
										}
										case "use" => {
											player.action(player.use,command.targets)
										}
										case "travel" => {
											player.action(player.travel,command.targets)
											interface.update(player.location)
										}
										case "inventory" => {
											player.showInventory(command.targets)
										}
										case "directions" => {
											print("option are:")
											for(m <- map.getLocationOptions(player.locationID))  print(" "+m)
											println("")

										}
										case "show" => {
											for(a <- command.targets; b <- characters if a.str == b.name) println(b.items)
										}
										case _ => interface.out("this verb is not defined ")

										}

									}// end iterate list
							}//end interpret

							def checkQuests(action: String){
								qTrigger();
								for (q <- openQuests){
									if(q.checkState(action)){
										openQuests -= q
												qTrigger();
									}
								}
							}

							def isOpenedQuests(i: Int): Boolean = {
									var j=0
											for(q <- closedQuests if q.id == i) j+=1
											j == 0
							}

							def isClosedQuests(i: Int): Boolean = {
									var j=0
											for(q <- closedQuests if q.id == i) j+=1
											for(q <- openQuests if q.id == i) j+=1
											j == 0
							}

							def openNewQuest(ii: Int){
								val temp = for(q <- closedQuests if q.id == ii) yield q
										openQuests ++= temp;
							}

							def playerOwns(item: String): Boolean = {
									player.checkInventory(item)
							}

							def playerGive(item: String) {
								player.inventory += item
							}

							def changeMap(a: Int) {
								map.changeMap(a)
							}

							def addItemToMap(a: Int, item: String) {
								map.addItemToMap(a,item)
							}

							def extraText(a: Int) {
								interface.out("extra text")
							}

							def getNpcLocation(a: Int) : Npc ={
									val nn = for(n <- characters if n.location == a) yield n
											if(nn.length >0){
												nn(0)
											}else{
												null
											}
							}

							def qTrigger(action: String = ""){
								if(playerOwns("time-machine")){
									openNewQuest(2)
								}
								if(action=="move24"){
									openNewQuest(3)
								}
								if(action=="move39" && isOpenedQuests(3)){
									openNewQuest(4)
								}
								if((action=="move52" || action=="move49") && isClosedQuests(3)){
									openNewQuest(5)
								}
								if((action=="move39" || action=="move54") && isClosedQuests(4)){
									openNewQuest(6)
								}
								if(playerOwns("card-1300") && isClosedQuests(3)){
									openNewQuest(7)
								}
								if(playerOwns("card-1300") && isClosedQuests(6)){
									openNewQuest(8)
								}
								if(isClosedQuests(7) && isClosedQuests(8)){
									openNewQuest(9)
								}
							}

}
