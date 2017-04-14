package scalaTest

import scala.collection.mutable.ArrayBuffer

class Player (val program: Program, interface : Interface,val map : MyMap, name: String = "guy", 
var location : String = "Forest Center 2014", var locationID : Int = 1) {

	var prevMovement="";
	var lastAction = "";
	val inventory = new ArrayBuffer[String]()
			val cardList = Array(("1700",38),("1300",49),("2014",1))
			val itemList = Array(("crowbar",0),("fish",0),("small rocks",0))

			println(s"you are in :$location ($locationID)")

			


			def suicide{
		interface.out("buuum")
		interface.out("you died!")
	}

	def checkInventory(item: String):Boolean = {
			var i=0
					for(b<-inventory if b == item) i+=1
					i!=0
	}


	def action(func: (ArrayBuffer[parser.Word with parser.order]) => Boolean, 
			aa : ArrayBuffer[parser.Word with parser.order]):Boolean = {
					val rr = func(aa)
							program.checkQuests(lastAction)
							rr

	}

	def relocate(temp: Int){
		locationID = temp
		try{location = map.getLocationName(locationID)}
		catch {case e: Throwable => println(locationID)}
				interface.out(s"moved to $location ($locationID)");
		interface.updateDescription(locationID)
		lastAction="move"+locationID
		
	}
	def move(a : ArrayBuffer[parser.Word with parser.order])={
			if(a.length == 0){
				interface.out("move where");
				false
			}else{
				val temp: Int = a(0).str match{
				case "north" => map.getLocationID(map.up,locationID);
				case "south" => map.getLocationID(map.down,locationID);
				case "east" => map.getLocationID(map.right,locationID);
				case "west" => map.getLocationID(map.left,locationID);
			}
			prevMovement = a(0).str

					if(locationID == temp){
						interface.out("cannot move there");
						false
					}else{
						relocate(temp);
						true
					}
			}
	}

	def kill(a : ArrayBuffer[parser.Word with parser.order])={
			interface.out("no time to kill now")
			true
	}

	def travel(a : ArrayBuffer[parser.Word with parser.order])={
			var j=0
					for(aa <- inventory if aa == "time-machine") { j+=1}
			if(j > 0){
				if(a.length > 0  ){
					var i=0

							var dest = ("",0)
							for(aa <- a; c <- cardList if aa.str == c._1) {dest=c; i+=1}
					if(i>1){
						interface.out("choose one card only")
					}else if(i==0){
						interface.out("travel where?")
					}
					else {
						var i=0
								for(aa<-a; b<-inventory if "card-"+aa.str == b) i+=1
								if (i != a.length){
									interface.out("you don't have these items")
								}else{

									
											interface.out("travelled to " + dest._1 +" id: " + dest._2)
											lastAction="travel"+dest._1
											relocate(dest._2)
								}
					}

				}else{
					interface.out("travel where?")
				}
			}else{
				interface.out("you don't have time-machine")
			}
			true
	}

	def use(a : ArrayBuffer[parser.Word with parser.order])={
			if(a.length > 0  ){
				var i=0
						for(aa <- a if aa.str == "time-machine") i+=1
						if(i>0){
							travel(a);
						}else{
							i=0
									var dest = ("",0)
									for(aa <- a; c <- itemList if aa.str == c._1) {dest=c; i+=1}
							if(i==0){
								interface.out("use what???")
							}else if(i==1){
								i=0
										for(aa<-a; b<-inventory if aa.str == b) i+=1
										if (i != a.length){
											interface.out("you don't have that item")
										}else{
											useAction(dest._1)
										}
							}else{
								interface.out("choose only one item to use")
							}
						}
			}
			true


	}

	def useAction(target: String){


		target match{
		case "crowbar" =>{
			if(locationID==4){
				interface.out("you smashed the lock")
				lastAction="use_crowbar";
				map.removeItems("lock")
			}else{
				interface.out("you cannot use crowbar now")
			}
		}
		case "fish" =>{
			interface.out("use fish for what??")
		}
		}
	}

	def eat(a : ArrayBuffer[parser.Word with parser.order])={
			interface.out("no time to eat now")
			true
	}

	def drink(a : ArrayBuffer[parser.Word with parser.order])={
			interface.out("no time to drink now")
			true
	}

	def speak(a : ArrayBuffer[parser.Word with parser.order])={

			if(a.length > 0 ){// if object was given
				val npc = program.getNpcLocation(locationID)
						if(npc!=null && npc.name == a(0)){
							interface.talkmode(npc, location + ".png")
						}else{interface.out("that person is not here!")}
			}else{
				interface.out("talk to who?")
			}
			true
	}

	def pick(a : ArrayBuffer[parser.Word with parser.order])={
			val list = map.getItemList(locationID)

					val hits = for(aa <- a; bb <- list if aa.str == bb._1) yield aa.str

					if(hits.length < 1 ){
						interface.out("take what?")
					}else{
						var i=0
								for(aa<- a if map.checkItems(aa.str, locationID)) i+=1
								if(i==a.length)
									for(aaa <- hits) {
										map.removeItems(aaa); 
										inventory.append(aaa); 
										interface.update_notification(s"took $aaa",5)
									}
								else{
									interface.out("there is no: ",false)
									for(aa<-a) {val ttt = aa.str; interface.out(s" $ttt",false)}
									interface.out(" in here")
								}


					}
			true
	}

	def drop(a : ArrayBuffer[parser.Word with parser.order])={

			for(b <- a) {map.addItems(b.str,locationID); inventory -= (b.str); interface.out(s"dropped $b")}
			true
	}

	def give(a : ArrayBuffer[parser.Word with parser.order])={

			val list = for( aa <- a; bb <- program.characterList if aa.str == bb) yield aa

					if(list.length==0){
						interface.out("give to who?")
					}else if(list.length>1){
						interface.out("choose only one person")
					}else{
						val target = list(0).str
								a -= (list(0))
								var i=0
								for(aa<-a; b<-inventory if aa.str == b) i+=1
								if (i == a.length){
									interface.out("you gave",false)
									for( aa <- a; cc <- program.characters if target == cc.name) {
										interface.out(" ", false)
										interface.out(aa.str, false)
										cc.addItem(aa.str)
										inventory -= aa.str
										lastAction="give_"+aa.str
										program.checkQuests(lastAction)
									}
									interface.out(s" to $target")
								}else{
									interface.out("you don't have:",false)
									for(aa<-a) {val ttt = aa.str; interface.out(s" $ttt",false)}
									interface.out(" in inventory")
								}
					}
			true
	}

	def showInventory(a : ArrayBuffer[parser.Word with parser.order])={
			interface.out("you have:")
			for(i <- inventory){
				interface.out(i)
			}
			true
	}

	def lookAround(a : ArrayBuffer[parser.Word with parser.order])={
			interface.out("you see:")
			val list = map.getItemList(locationID)
			println(list)
			true

	}
	
	







}