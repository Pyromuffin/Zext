package Game

import Zext.*
import Zext.exports.*

object Town extends Room{

  name = "pelican town square"
  description = "Pelican town needs no introduction. You've been through the square countless times while accomplishing the tasks required by farm life"

  report(being, here, Randomly(4), visited) Say "Mayor Lewis peers at you expectantly, waiting for your permission to start the event"

  report(entering, here, !visited) Say "Mayor Lewis shouts: \"Gather 'round everyone! The egg hunt is starting soon!\""


  Connect(west, Path)

}
