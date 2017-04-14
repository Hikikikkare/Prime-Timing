package parser

import scala.collection.mutable.ArrayBuffer

class Word(protected var _str: String) {
	def shout() {println(str)}
	def str = _str
			def str_=()={
					_str
	}

	override def toString = {
			str
	}

}

trait order{
	var index = 0
			def shoutIndex() {println(index)}
}

trait condition{
	var name: String
	def shoutCondition() {println(name)}
}

class Verb(str: String) extends Word(str){
	def this(w : Word){
		this(w.str)
	}

}

class Subs(str: String) extends Word(str){

}

class Misc(str: String) extends Word(str){

}

class Command(private var _verb: Verb, var targets: ArrayBuffer[Word with order]){

	def this(v : Verb){
		this(v, new ArrayBuffer[Word with order])
	}
	var i = -1
			for (a <- targets) {i+=1; a.index=i}
	def verb = _verb

}