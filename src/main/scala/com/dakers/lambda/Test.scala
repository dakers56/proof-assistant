package com.dakers.lambda

object Test {


  def main(args: Array[String]): Unit = {

    +~("x".decl() * "y".decl())
    +~("x".decl())
    for ((k,v) <- context) printf("key: %s, value: %s\n", k, v)

  }


}
