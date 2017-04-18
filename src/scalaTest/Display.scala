package scalaTest


import javax.swing._
import java.awt._
import java.awt.event._
import javax.imageio.ImageIO
import java.io.File
import java.awt.Image._
import java.awt.image._
import scala.collection.mutable.ArrayBuffer
import java.awt.Font._
import java.awt.font._
/*
 * sets up the canvas and overrides paint component method
 * 
 * images 					=	array that holds the images to be drawn in the order they are to be drawn
 * written_letter		= the char from user who types on the screen
 * drawable_string	=	string to be drawn on the screen 
 * row_height				=	when the text is multiple rows this variable defines row height
 * drawable_string_location = where the string to be drawn is to be drawn
 */

trait helpful{
	val window_size=(1000,600)
			val rulla_size = %%(30,50)//percentage
			val rulla_location = %%(70,10)//percentage
			val speech_bub_size = %%(30,40)//percentage
			val speech_bub_location = %%(30,20)//percentage
			val laatikko_size = %%(100,20)//percentage
			val laatikko_location = %%(0,80)//percentage
			val talkmode_speech_location = (rulla_location._1 ,rulla_location._2)
			val talkmode_answer_location = ((speech_bub_location._1 + (speech_bub_size._1/10)).toInt,0)
			val notification_size = %%(40, 20)//percentage
			val notification_location = %%(30,30)//percentage
			val font_style = "Serif"
			val font_size=((window_size._1+window_size._2)/75).toInt
			def percentasize(percentage : Int, of : Int): Int ={

					var a = percentage : Double
							a = (a/100) * of
							a.toInt
	}

	def %% (point : (Int,Int)) : (Int,Int) ={
			var point_x = 0
					var point_y = 0
					if(point_x > 100)point_x = window_size._1 else point_x = percentasize(point._1,window_size._1)
					if(point_y > 100)point_y = window_size._2 else point_y = percentasize(point._2,window_size._2)
					(point_x,point_y)
	}
}

class MyPanel extends JPanel with helpful{
	var images = new ArrayBuffer[(String,(Int,Int),(Int,Int))]
			var image = new BufferedImage(window_size._1,window_size._2,BufferedImage.TYPE_INT_ARGB)//ImageIO.read(new File("images/kuva1.png"))
			var previous_image=""
			var written_letter = ""
			var drawable_string = new ArrayBuffer[(String,Color)]
					var strings = new ArrayBuffer[((String,Color),(Int,Int))]
							var drawable_string_location = (0,0)
							val row_height = 20
							//var canvas = new Canvas(image.createGraphics())
							var dispose_flag = false
							var paint_flag = false 
							var notifications_pending = false
							var autoclear = true
							var clear_flag= true
							var notification_text = ""
							var myfont = new java.awt.Font(font_style, java.awt.Font.PLAIN, font_size)
							val paint_thread = new Thread {
		override def run {
			while(true){
				//if(paint_flag){
				if(clear_flag){
					fill_image(image.createGraphics())
					if(notifications_pending){
					  put_notification_to_image(image.createGraphics())
					  notifications_pending = false
					}
					//paint_flag=false
					//draw_flag=true
					synchronized(set_clear_flag(false))
					repaint()
				}
				//}
				Thread.sleep(10)
			}
		}
	}

	def set_clear_flag(b : Boolean){
		clear_flag= b
	}

	/*
	 * setter for images ArrayBuffer
	 */
	def setframe(images1 : ArrayBuffer[(String,(Int,Int),(Int,Int))]){

		if(images1.length > 0){
			if(exists(images1(0)._1)){
				previous_image=images1(0)._1
			}

			val drawable = new ArrayBuffer[(String,(Int,Int),(Int,Int))]
					for (i <- images1){
						drawable.append(i)
					}
			images = drawable
		}
	}
	/*
	 * check if file exists
	 */
	def exists(image_file : String): Boolean ={
			var ret = true
					try{
						ImageIO.read(new File("images/" + image_file))
						ret = true
						ret
					}catch{
					case e: javax.imageio.IIOException => ret = false;ret
					}
	}
	/*
	 * setter for the drawable_string
	 * each row is in a element
	 * when the line is cut in middle of a word it seeks the white space before it and puts the line division in there
	 */
	def set_drawable_string(str: String, point1:(Int,Int),row : Int, color: Color){
		var words = str.split('\n')
		var point = point1
		for(wrd <- words){
			if(wrd!="null"){
						var amount_of_rows = wrd.length() / row
						amount_of_rows = amount_of_rows.toInt
						var new_line = 0
				var old_line = 0
						if(amount_of_rows >= 1){
							for(i <- 0 to amount_of_rows -1){
								new_line = row
										while(wrd.charAt(old_line+new_line) != ' '){
											new_line -= 1
										}
								new_line += 1
								strings.append(((wrd.substring(old_line,(old_line+new_line)),color),point))
								point = (point._1,point._2 + font_size)
								old_line += new_line
							}
							
							strings.append(((wrd.substring(old_line),color),point))
						}else{
							strings.append(((wrd,color),point))}
						//println("point = " + point)
			}
			
		}
	}

	/*
	 * sets drawable char received from keyboard
	 */
	def setchar(letter1 : String){
		var asd = letter1.charAt(letter1.length()-1) :Int
				if(asd == 8 && written_letter.length > 0){written_letter=written_letter.substring(0, written_letter.length() - 1)}
				else{ written_letter+=letter1}
	}
	
	def set_pending_notification(notification_text1 : String){
	  notification_text = notification_text1
	  notifications_pending=true
	}
	
	def put_notification_to_image(g:Graphics2D){
	  var image1 = ImageIO.read(new File("images/dialogi.png"))
	  g.drawImage(image1, notification_location._1, notification_location._2, notification_size._1,notification_size._2 , null)
	  g.setColor(Color.BLACK)
	  g.setFont(myfont)
	  g.drawString(notification_text, notification_location._1+20, notification_location._2 + 40)
	}


	def fill_image(g : Graphics2D){
	  
		for(imageA <- images){
		  var image1 = new BufferedImage(10,10,BufferedImage.TYPE_INT_RGB)
		  try{
						image1 = ImageIO.read(new File("images/" + imageA._1))
					}catch{
					case e: javax.imageio.IIOException => image1= ImageIO.read(new File("images/" + previous_image))
					}
				g.drawImage(image1, imageA._2._1, imageA._2._2, imageA._3._1, imageA._3._2, null)
				//image = image1

		}
		if(written_letter !=""){

			g.setColor(Color.WHITE)
			g.drawString(written_letter, 50,500)
		}
		
			var i = 0
			if(strings.length >0){
			for(string <-strings){
			  if(string._1._1 != ""){
						g.setColor(string._1._2)
						g.setFont(myfont)
						g.drawString(string._1._1, string._2._1, string._2._2)
						i = i +1
			}
			//strings.clear()
			}
		}
			if(autoclear){
			  if(clear_flag){
			    strings.clear()
			  }
			}
			
		
	}
	/*
	 * this is called with repaint()
	 * draws everything on the screen
	 */
	override def paintComponent(g : Graphics){
		super.paintComponent(g)
		//paint_flag=true
		//println("new")
		/*if(dispose_flag){
					g.dispose()
					dispose_flag=false
				}*/
		g.drawImage(image, 0, 0, window_size._1, window_size._2, null)
	}

}
/*
 * the actual window frame that holds the canvas
 * window_size	= the size of the created window
 * panel				=	MyPanel class ( canvas )
 * 
 */
class Display extends JFrame() with helpful{

	val frame = new JFrame("Display");
	val canvas = new Canvas()

			var panel= new MyPanel()


			var image_infoarray = new ArrayBuffer[(String,(Int,Int),(Int,Int))]



					def setup_GUI(background : String ="Home.png"){

		panel.paint_thread.start()
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.add(panel)
		frame.pack();
		frame.setVisible(true);
		frame.setSize(window_size._1, window_size._2)
		//draw(background, (0,0))
	}
	def update_i(){
		panel.paint_flag =true
				//frame.repaint()
	}

	/*
	 * draws on top of the current image
	 * appends the image on the MyPanel images ArrayBuffer and when repaint() is called everything on the arrayBuffer is drawn
	 */
	def draw_on_top(filename : String, point : (Int,Int),size : (Int,Int)){
		var size_x =size._1
				var size_y =size._2
				if((point._1 + size._1) > window_size._1)size_x = size._1 - (point._1 - window_size._1)
				if((point._2 + size._2) > window_size._2)size_y = size._2 - (point._2 - window_size._2)
				//println("image_infoarray.append((" + filename + ",(" + point._1 + "," + point._2 + "),(" + size._1 + "," + size._2 + ")))")
				image_infoarray.append((filename,(point._1,point._2),(size_x,size_y)))
				panel.setframe(image_infoarray)

				//frame.repaint()
	}


	/*
	 * draws entirely new frame with the black talkbox
	 */
	def draw(filename : String, point : (Int,Int)=(0,0)){
		//panel.strings.clear()
		panel.dispose_flag=true
				panel.drawable_string.clear()
				image_infoarray.clear()
				image_infoarray.append((filename,point,(frame.getWidth,frame.getHeight)))
				draw_on_top("laatikko.png",(laatikko_location),(laatikko_size))
	}
	/*
	 * sets a single character to var letter in MyPanel class
	 */
	def draw_text(ch : Char){
		panel.setchar(ch.toString())
		//panel.paint_flag =true
		//frame.repaint()
	}
	/*
	 * sets drawable_string to MyPanel object
	 * draws the images array and the text on top
	 */
	def draw_string(str : String,point : (Int,Int), row_length : Int, color :Color = Color.BLACK){
		panel.set_drawable_string(str, point,row_length,color)
		//panel.paint_flag =true
		//frame.repaint()
	}
	/*
	 * returns the given command in string 
	 * ex. "kill the rapist with the beautiful sword"
	 */
	def get_written()={
			val com = panel.written_letter
					com
	}

}












/*
import java.awt._
import java.awt.Canvas
import javax.swing._

import scala.collection.mutable.ArrayBuffer

class Display extends JFrame {
	// Define named-constants
	val CANVAS_WIDTH = 640;
	val CANVAS_HEIGHT = 480;


	var imageBuffer = ArrayBuffer[(Image,Int,Int,String)]()





	// Constructor to setup the GUI components and event handlers

	var panel = new Panel(); // the drawing canvas (an inner class extends JPanel)
	panel.setPreferredSize(new Dimension(CANVAS_WIDTH, CANVAS_HEIGHT));
	this.setContentPane(panel);
	//this.setDefaultCloseOperation(EXIT_ON_CLOSE);
	this.pack();
	this.setTitle("Test display");
	this.setVisible(true);


	def update() {

	}

	def addImage(img: Image, x: Int, y: Int, str: String = ""){
	  imageBuffer.append((img, x, y, str))
	}


	// Define Inner class DrawCanvas, which is a JPanel used for custom drawing
	class Panel extends JPanel {

		override def paintComponent(g: Graphics) {
			super.paintComponent(g);  // paint parent's background
			setBackground(Color.PINK);

			for(img <- imageBuffer){
			  g.drawImage(img._1,img._2,img._2,null)
			}

			imageBuffer.clear()

			   /*
					var c = new Canvas()
					c.setSize(200,200)
					var gg = c.getGraphics()



					if(gg != null){

						gg.setColor(Color.BLACK)
						gg.fillRect(0, 0, 100, 100)

						gg.setColor(Color.BLUE)
						gg.fillRect(100, 100, 200, 200)

						var ii = c.createImage(200, 200)
						g.drawImage(ii, 100, 100, null)
					}else{
					  println("it is nulll")
					}

 */



		}
	}


}*/