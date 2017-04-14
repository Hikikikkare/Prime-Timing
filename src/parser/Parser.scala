package parser

import scalaTest.Database
import scala.collection.mutable.ArrayBuffer

class Parser {
	/*
	val verbs = Array("move", "eat", "kill", "and")
			val subs = Array( "north", "south", "west", "east", "apple", "dragon", "ogre")
			val misc = Array( "is")
			var comm = Map( "go" -> "move", "travel" -> "move", "walk" -> "move", "slay" -> "kill", "," -> "and", "s" -> "is",
					"drake" -> "dragon")
	 */

	//      val verbs = scalaTest.SQLCommunicator.query("word", "verbs")
	//      val subs = scalaTest.SQLCommunicator.query("word", "subs")
	//      val misc = scalaTest.SQLCommunicator.query("word", "misc")

	val simpleCommands = Array("b","back")

			val verbs = Database.query("word", "verb")
			val subs = Database.query("word", "subs") ++ Database.query("name", "objects")  ++ 
			Database.query("name", "speaks")
			val misc = Database.query("word", "misc")

			val wrong = Database.query("wrongname", "synonyymi")
			val right = Database.query("rightname", "synonyymi")
			val comm = for(i <- 0 to wrong.length-1) yield (wrong(i),right(i))
			/*for(i <- 0 to wrong.length){
        comm.+(wrong(i), right(i))
      }*/

			//val comm = scalaTest.SQLCommunicator.query("word", "synonyymi")

			var origSentence = ArrayBuffer[Word]()




			def getType(a: Any) ={

					a match{
					case v:Verb => 'v'
					case s:Subs => 's'
					case m:Misc => 'm'
					case _ => 'n'
					}
			}

	/*
	 * check if words are directly detected
	 * 
	 */
	def checkWord(word: String, sentence: ArrayBuffer[Word with order], i:Int )={


			if(verbs.contains(word)) {sentence.append({val v = new Verb(word) with order ; v.index=i; v}); true}
			else if(subs.contains(word))  {sentence.append({val v = new Subs(word) with order ; v.index=i; v}); true}
			else if(misc.contains(word))  {sentence.append({val v = new Misc(word) with order ; v.index=i; v}); true}
			else false
			}

	/*
	 * check quicklist for alternative commands
	 */
	def quickMatch(word: String )={
			var found = false
					var m = word
					for(c <- comm; if !found){
						if(word == c._1) { // word matches alternative commands, change value and save
							found = true
									m=c._2
						}
					}
			m
			}

	/*
	 * separate words from input
	 * save results to origSentence
	 */
	def separateWords(command: String){

				origSentence.clear() // clear old sentence

				var str = ""
				for (chr <- command){
					if(chr==' ' || chr == '\n' || chr == '\''){
						if(str.length() > 0) origSentence.append( new Word(str) ); str=""; 

					}else str+=chr

				}
				if(str.length>0) origSentence.append(new Word(str))
			}

	/*
	 * translates input to commands
	 */
	def parse(command: String, qu : Boolean)={
			var commands = new ArrayBuffer[Command]()
					if(qu){// if no need for parsing, just detect simple commands
						var i =0
								for(s <- simpleCommands if command== s) i+=1
								if(i==0){
									commands += new Command(new Verb("unkn"))
								}else if(i>1){
									commands += new Command(new Verb("toom"))
								}else{
									command match{
									case "b" => commands += new Command(new Verb("back"))
									case "back" => commands += new Command(new Verb(command))
									case _ => println("error: quickparse")

									}
									commands += new Command(new Verb("too"))
								}
					} else{

						var sentence = ArrayBuffer[Word with order]()

								/*
								 * separate words from input
								 * save results to origSentence
								 */
								separateWords(command)

								var i=0
								var j=0
								for (word <- origSentence){ // iterate all found words

									/*
									 * check if words are directly detected
									 */
									if(!checkWord(word.str, sentence, i)){

										/*
										 * if not found, check quick list
										 */

										val quickRes = quickMatch(word.str)
												if(word.str != quickRes){
													if(!checkWord(quickRes, sentence, i)){
														println(s"error: parser->parse !check lists1!: $quickRes")
													}
												}
										/*
										 * if not found, check multi words (ie. fish-eye steve)
										 */
												else{ 
													var found=false
															/*
															 * two words
															 */
															if(origSentence.length > j+1 ){
																var tword = word.str + " " + origSentence(j+1)
																if(!checkWord(tword, sentence, i)){
																	val quickRes = quickMatch(word.str)
																			if(word.str != quickRes){
																				found=true
																						if(!checkWord(quickRes, sentence, i)){
																							println(s"error: parser->parse !check lists2!: $quickRes")
																						}
																			}
																}else{
																	found=true
																}
															}

													/*
													 * three words
													 */
													if(!found && origSentence.length > j+2 ){
														var tword = word.str + " " + origSentence(j+1)+ " " + origSentence(j+2)
														if(!checkWord(tword, sentence, i)){
															val quickRes = quickMatch(word.str)
																	if(word.str == quickRes){
																		if(!checkWord(word.str, sentence, i)){
																			println("error: parser->parse !check lists!3")
																		}
																	}
														}
													}


													i-=1
												}
									}
									i+=1
											j+=1

								}//end iterate all found words



						var verbNumbers = new ArrayBuffer[Int]()

								for (a <-  sentence if (getType(a) == 'v') ){
									commands.append(new Command(new Verb(a)))
									verbNumbers.append(a.index)
								}

						if(commands.length>0){
							if(commands.length==1){
								commands(0).targets = for (a <- sentence if (getType(a) != 'v')) yield a
							}
							else {
								var i=0
										for(c <- commands){

											/*c.targets = for (i <- 0 to sentence.length
								if (getType(sentence(i)) != 'v' && 
								sentence(i).index < verbNumbers(i))
								)	yield sentence(i)*/

											c.targets = for (a <- sentence
													if (getType(a) != 'v' && 
													a.index < verbNumbers(i))
													)	yield a

													i+=1
										}
							}
						}	
					}
			commands
	}//end parse



}




