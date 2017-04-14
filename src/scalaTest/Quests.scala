package scalaTest


object Quests{


	val amount = 9

			val questTriggers1  = Map(1 -> 2 )
			val questTriggers2 = Map(1 -> 3)
			val questTriggers3 = Map(1 -> 3)
			val questTriggers4 = Map(1 -> 3)
			val questTriggers5 = Map(1 -> 3)
			val questTriggers6 = Map(1 -> 3)
			val questTriggers7 = Map(1 -> 3)
			val questTriggers8 = Map(1 -> 3)
			val questTriggers9 = Map(1 -> 3)

			val questTriggers = (questTriggers1,questTriggers2,questTriggers3,questTriggers4,
					questTriggers5,questTriggers6,questTriggers7,questTriggers8,questTriggers9)

			def apply(pr: Program,i : Int, str: String = "qx")={
					new Quests(pr,i,str)
	}

}

class Quests(val program: Program, val id: Int , val name: String){


	var state=1


			def checkState(action: String)={
					id match{
					case 0 => q1(action)
					case 1 => q2(action)
					case 2 => q3(action)
					case 3 => q4(action)
					case 4 => q5(action)
					case 5 => q6(action)
					case 6 => q7(action)
					case 7 => q8(action)
					case 8 => q9(action)
					}
	}

	//case _ => if(Quests.questTriggers._1.get(state) == action) state+=1

	def next(i: Int = 0){
	  println("next:"+id+" text:"+i)
		state+=1
				if(i!=0)
					program.extraText(i);
	}

	/*
	 * 1.take time machine
    0.start game
    1.take crowbar
    1.take coating
    
    3.break lock
    4.take time machine
	 */


	def q1(action: String)={
			var ret = false
					state match{
					case 1 => if(program.playerOwns("crowbar")){
						next()
					}case 2 => if(action=="use_crowbar"){
					  program.changeMap(58)
						next()
					}case 3 => if(program.playerOwns("coating")){
					  program.addItemToMap(7, "time-machine")
						next()
					}case 4 => if(program.playerOwns("time-machine")){
						program.playerGive("card-1700")
						program.changeMap(8)
						ret = true
					}case _ => ;
			}
			ret
	}

	/*
	 * 2.go home
    0.time machine = backpack <- q1.3
    1.go to main street
    2.go home
    3.use time machine (press button)
    4.chase the thief
	 */
	def q2(action: String)={
			var ret = false
					state match{
					case 1 =>  if(program.playerOwns("time-machine")){
						next()
					}case 2 => if(action=="travel1700"){
						next()
					}case 3 => if(action=="move34"){
						next()
					}case 4 => if(action=="move17"){
						program.playerGive("locked box")
						program.changeMap(3)
						next(2) //extra text
						ret=true
					}case _ => ;
			}
			ret
	}

	/*
3.getting card 14
    0.oma sijainti = 24
    1.kill a bear
    2.give watch to vendor
	 * */
	def q3(action: String)={
			var ret = false
					state match{
					case 1 =>{ 
					  if(action=="killBearFish"){ // 2 missing???????????  what????????+
					    program.addItemToMap(32, "meat")
					    program.addItemToMap(32, "pocketwatch")
						next(-2)
						}else if (action=="killBearRocks"){
						  program.addItemToMap(32, "meat")
					    program.addItemToMap(32, "pocketwatch")
						  next(-1)
						}
					}case 2 => if(action=="move17"){ // 2 missing???????????  what????????+
						program.playerGive("card-1300")
						next(-3)
						ret=true
					}case _ => ;
			}
			ret
	}

	/*
4.enemy
    0.oma sijainti=39 ja 3 on valmis
    1.go to 1300
    2.talk to king
    3.go to enemy camp
	 */
	def q4(action: String)={
			var ret = false
					state match{
					case 1 => if(action=="move38"){
						next()
					}case 2 => if(action=="talkKing1b"){ 
					  //change king speak mode
						next()
					}case 3 => if(action=="move39"){ 
						next(-4)
						ret=true
					}case _ => ;
			}
			ret
	}

	/*

5.poisoning drinks
    0.oma sijainti = 49 
    1.go to 1750 inn and get wine
    2.get deer horns 
    3.buy venom 
    4.poison drinks
	 */
	
	def q5(action: String)={
			var ret = false
					state match{
					case 1 => if(action=="move44" && program.playerOwns("wine barrels")){ 
						next()
					}case 2 => if(program.playerOwns("3 deer horns")){ 
						next()
					}case 3 => if(program.playerOwns("venom")){ 
						next()
					}case 4 => if(program.playerOwns("poisoned wine barrels")){ 
						
						ret=true
					}case _ => ;
			}
			ret
	}
	
	/*
6.getting key
    0.4 tehty oma sijainti = 39 tai 54
    1.go to enemy camp
    2.go to 1750 castle and break showcase
    3.take key
	 */
def q6(action: String)={
			var ret = false
					state match{
					case 1 => if(action=="move39" && program.playerOwns("poisoned wine barrels")){ 
					  //change king speak to 3
						next()
					}case 2 => if(action=="move36"){ 
					  program.addItemToMap(36, "key2")
						next()
					}case 3 => if(program.playerOwns("key2")){ 
						ret=true
					}case _ => ;
			}
			ret
	}
	/*
7.chopping down tree 
    0.kortti 14 ja 3 tehtynä
    1.talk to lumberjacks1300
    2.get squirrelfur
    3.talk to vendor and get nice clothes
    4.give nice clothes to lumberjacks
    5.take key2 from the chopped down tree
	 */
def q7(action: String)={
			var ret = false
					state match{
					case 1 => if(action=="talkLumber1"){ 
						next()
					}case 2 => if(program.playerOwns("squirrel fur")){ 
						next()
					}case 3 => if(program.playerOwns("nice clothes")){ 
						next()
					}case 4 => if(action == "give_nice clothes"){ 
						next()
					}case 5 => if(program.playerOwns("key1")){ 
						
						ret=true
					}case _ => ;
			}
			ret
	}
	/*

8.key3
    0. 6 tehty ja kortti14
    1.beat up fish eye steve 1300 after phase 7.1
    2.talk to king 1300
    3.give bear meat to tyrone
    4.rob shovel from vendor
    5.dig hole
    6.go and get te key from location 57

	 */

def q8(action: String)={
			var ret = false
					state match{
					case 1 => if(action=="attackSteve" && program.isOpenedQuests(7)){ 
						next()
					}case 2 => if(action=="talkKing3"){
						next()
					}case 3 => if(action == "give_meat"){
						next()
					}case 4 => if(program.playerOwns("shovel")){
						next()
					}case 5 => if(action == "useShovel"){
						next()
					}case 6 => if(program.playerOwns("key3")){
						
						ret=true
					}case _ => ;
			}
			ret
	}

	/*
9.epilogue
    1. get everything
    2. open box
    3. use timemachine 2014
	 */
def q9(action: String)={
			var ret = false
					state match{
					case 1 => if(program.playerOwns("key1")&&program.playerOwns("key2")&&program.playerOwns("key3") ){ 
						next()
					}case 2 => if(program.playerOwns("card-2014")){ 
						next()
					}case 3 => if(action == "travel2014"){ 
						next(-4)
						ret=true
					}case _ => ;
			}
			ret
	}

}

