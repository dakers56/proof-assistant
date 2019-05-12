package com.dakers.lambda

import scala.collection.mutable.ListBuffer

class DerivationContext(val varNames: Set[String] = Set(), val statements: ListBuffer[STTerm] = scala.collection.mutable.ListBuffer())