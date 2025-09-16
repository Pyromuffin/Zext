package Zext

import Zext.*
import Zext.exports.*



abstract class Person(using Container) extends ZextObject with Container {

  this is proper

  // uhh should people be containers?
  automaticallyListContents = false
  open = false
  transparent = false

}

