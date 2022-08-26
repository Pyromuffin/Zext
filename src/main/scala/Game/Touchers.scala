package Game

import Zext.{ZextObject, Actions, Macros}

object Touchers {
  Macros.TouchEveryone(Actions)
  Macros.TouchEveryone(ZextObject)

  // Macros.packageToucher(JunimoGame) // leave this here for now, maybe not the best place for it.

}
