package scalaTest

import scala.collection.mutable.ArrayBuffer

class MyMap() {



	var map1 = Database.query ("idlocation", "checkway")
			var map2 = Database.query ("idnorth", "checkway")
			var map3 = Database.query ("idsouth", "checkway")
			var map4 = Database.query ("idwest", "checkway")
			var map5 = Database.query ("ideast", "checkway")

			var map = new ArrayBuffer[(Int,Int,Int,Int,Int)]()
			for(i <- 0 to map1.length -1) map .append( (map1(i).toInt, map2(i).toInt,
					map3(i).toInt, map4(i).toInt, map5(i).toInt) )

			val blockedloc = Database.query("locked", "location")
			val blockedid = Database.query("idlocation", "location")

			val blocked = new ArrayBuffer[Int]
					for(aa <- 0 to blockedid.length -1 if (1 == blockedloc(aa).toInt)) blocked += blockedid(aa).toInt



					val items1 = Database.query ("name", "objects")
					val items2 = Database.query ("idobject", "objects")
					val items3 = Database.query ("location", "objects")

					var items:Map[String,Int] = Map()
					for(i <- 0 to items1.length-1)  
						items += ( items1(i) -> items3(i).toInt )

						items += ("door nob"->4)


						/*val items = new ArrayBuffer[(Int, String, Int)]()
							for(i <- 0 to items1.length-1)  
								items.append((items2(i).toInt,items1(i),items3(i).toInt))

								items.+=((40,"door",4))
						 */

						def findValue(f: ((Int, Int, Int, Int, Int)) => Int, loc: Int)={
								val v = for(m <- map if m._1==loc) yield m;
										if(v.length > 1){
											println(s"Error:: too many hits! -from $loc to $v")
										}

										if(v.length != 0) if( v(0)._1 != 0){
											val vv = f(v(0))
													if(vv==0) loc
													else vv
										}else{
											loc
										}
										else loc
					}
					def up(m: (Int, Int, Int, Int, Int))={
							m._2
					}
					def down(m: (Int, Int, Int, Int, Int))={
							m._3
					}
					def left(m: (Int, Int, Int, Int, Int))={
							m._4
					}
					def right(m: (Int, Int, Int, Int, Int))={
							m._5
					}

					def getLocationID(func: ((Int, Int, Int, Int, Int)) => Int, location: Int)={
							findValue(func, location)
					}

					def getLocationName(loc: Int)={
							Database.query("name", "location", "idlocation = "+loc)(0)
					}

					def getLocationOptions(loc: Int)={
							val v = for(m <- map if m._1==loc) yield m;
									val ab = new ArrayBuffer[String]
											if (v(0)._2 != 0) ab += "north"
											if (v(0)._3 != 0) ab += "south"
											if (v(0)._4 != 0) ab += "west"
											if (v(0)._5 != 0) ab += "east"
											ab
					}

					def getItemList(id: Int)={
							for(m <- items if m._2==id) yield m;
					}

					def removeItems(name : String ){
						items -= (name)
					}

					def addItems(name : String, loc : Int ){
						items += (name -> loc)
					}

					def checkItems(name : String, loc : Int )={
							var i=0
									for(a <- items if name == a._1) i+=1
									i>0
					}

					def isBlocked(loc : Int )={
							var i=0
									for(a <- blocked if loc == a) i+=1
									i>0
					}

					def changeMap(a: Int){

						val replacement = Database.query("new","checkupdate","old="+a)(0)
								for(i <- 0 to map.length-1){
									if(map(i)._2==a) map(i)=(map(i)._1,replacement.toInt,map(i)._3,map(i)._4,map(i)._5)
											if(map(i)._3==a) map(i)=(map(i)._1,map(i)._2, replacement.toInt,map(i)._4,map(i)._5)
											if(map(i)._4==a) map(i)=(map(i)._1,map(i)._2,map(i)._3, replacement.toInt, map(i)._5)
											if(map(i)._5==a) map(i)=(map(i)._1,map(i)._2,map(i)._3,map(i)._4,replacement.toInt)
								}
					}

					def addItemToMap(a: Int, item: String){
						items += (item->a)
					}
					
					
  def create_npcs()={
    val b = Database.query("idlocation, name", "characters")
    val npclist= new ArrayBuffer[Npc]
    npclist.append(new Npc(b(1),b(0).toInt))
    for(i <- 2 to b.length - 1 by 2){npclist.append(new Npc(b(i+1),b(i).toInt))}
    npclist
  }




}






