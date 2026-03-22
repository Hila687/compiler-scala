// Hila Miller 327737201
// Hila Rosental 215134842
// group 42

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.io.StdIn


object Tar0 {

  var totalBuy: Double = 0.0
  var totalSell: Double = 0.0

  def main(args: Array[String]): Unit = {

    val folderPath =
      if (args.length > 0) args(0)
      else {
        println("Enter folder path:")
        StdIn.readLine()
      }

    val folder = new File(folderPath)

    val folderName = folder.getName
    val outputFilePath = folder.getAbsolutePath + File.separator + folderName + ".asm"

    val writer = new PrintWriter(outputFilePath)

    val files = folder.listFiles()

    if (files != null) {
      for (file <- files) {
        if (file.isFile && file.getName.endsWith(".vm")) {
          processFile(file, writer)
        }
      }
    }

    val buyLine = "TOTAL BUY: " + totalBuy
    val sellLine = "TOTAL SELL: " + totalSell

    println(buyLine)
    println(sellLine)

    writer.println(buyLine)
    writer.println(sellLine)

    writer.close()
  }

  def processFile(file: File, writer: PrintWriter): Unit = {

    val fileNameWithoutExtension = removeVmExtension(file.getName)
    writer.println(fileNameWithoutExtension)

    val source = Source.fromFile(file)

    for (line <- source.getLines()) {
      val words = line.trim.split("\\s+")

      val command = words(0)
      val productName = words(1)
      val amount = words(2).toInt
      val price = words(3).toDouble

      if (command == "buy") {
        HandleBuy(productName, amount, price, writer)
      } else if (command == "cell") {
        HandleSell(productName, amount, price, writer)
      }
    }

    source.close()
  }

  def HandleBuy(productName: String, amount: Int, price: Double, writer: PrintWriter): Unit = {
    val sum = amount * price
    totalBuy += sum

    writer.println(s"### BUY $productName ###")
    writer.println(f"$sum%.1f")  }

  def HandleSell(productName: String, amount: Int, price: Double, writer: PrintWriter): Unit = {
    val sum = amount * price
    totalSell += sum

    writer.println("$$$"+ s" SELL $productName " + "$$$")
    writer.println(f"$sum%.1f")  }

  def removeVmExtension(fileName: String): String = {
    if (fileName.endsWith(".vm")) {
      fileName.substring(0, fileName.length - 3)
    } else {
      fileName
    }
  }
}
