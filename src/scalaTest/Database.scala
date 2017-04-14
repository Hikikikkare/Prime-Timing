package scalaTest


import java.sql._
import java.sql.DriverManager

import scala.collection.mutable.ArrayBuffer


import org.sqlite._
import org.sqlite.core._
import org.sqlite.date._
import org.sqlite.javax._
import org.sqlite.jdbc3._
import org.sqlite.jdbc4._
import org.sqlite.util._


object Database {

  val dbUrl = "jdbc:sqlite:database.db"
	val connection = DriverManager.getConnection(dbUrl)
	val statement = connection.createStatement()
	
	
  
	
	
	def getValue( column: String, table: String, where: String = "") = {
    if (where != "")
    statement.executeQuery(s"select $column from $table where $where;")
    else
      statement.executeQuery(s"select $column from $table;")
    
  }
  
  def getNumber(table: String, column: String, where: String) = {
    getValue(table,column,where)
    10
  }
  
  def apply(column: String, table: String, where: String = ""){
    
    try{
      println("HERE?")
      println(s"select $column from $table where ($where);")
      query(column,table,where)
    }catch{
      
      case e: Exception => println("exception caught: " + e)
      case e: Throwable => println("exception caught: " + e)
    }
    query(column,table,where)
    
  }
  
  def query(select: String,from :String, where : String="")={
    var ret = new ArrayBuffer[String]
    val statement = connection.createStatement()
    var resultSet = statement.executeQuery("select " + select + " from " + from)
    
    if(where != ""){resultSet = statement.executeQuery("SELECT " + select + " FROM " + from + " WHERE " + where)}//
    
    if(select == "*"){
    val md = resultSet.getMetaData(); 
    val colCount = md.getColumnCount();  
    for (i <- 1 to colCount){  
    val col_name = md.getColumnName(i);  
    ret.append(resultSet.getString(col_name))
    }
    }else if(select.contains(",") ){
    val selects = select.split(", ")
    
      while ( resultSet.next() ) {
        for(sel <- selects){
        ret.append(resultSet.getString(sel))
        }
      }
    }else{   
     while ( resultSet.next() ) {
        ret.append(resultSet.getString(select))
      }
    }
    ret
  }

}