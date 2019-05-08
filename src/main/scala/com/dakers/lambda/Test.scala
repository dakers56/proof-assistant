package com.dakers.lambda

object Test {
  def main(args: Array[String]): Unit = {
    println("Testing lam deserialization function")
//    println("lam(x): " + lam("x"))
    println("lam(/|x.x): " + lam(s"$AbstOp"+"x.x"))
//    println("lam(xy): " + lam("xy"))
    println("lam(/|x.xy): " + lam("/|x.xy"))
  }
}
