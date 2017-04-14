
package scalaTest

import scala.io._

class Runner extends Thread{
  var  running =true
  override def run(){
    while(running){
      
      Thread.sleep(1000)
      synchronized(doit("child"))
      //doit("child")
    }
  }
  
  def doit(str : String){
    println(str+" in")
    Thread.sleep(3000)
    println(str+" out")
    
  }
  
}

object MainClass extends App {
  
  var interface = new Interface()
  var program = new Program(interface)
  
  //val runner = new Runner;
  //runner.start();
  
  while(true){
    
    val command = interface.read
    
    program.update(command)
    
    Thread.sleep(30)
    
    
    
    //synchronized(runner.doit("main"))
    //Thread.sleep(1000)
  }
}