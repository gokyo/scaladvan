package simulations

import math.random

class EpidemySimulator extends Simulator {

  // random number in [0, i)
  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val deathRate = 0.25

    val incubationTime = 6
    val deathTime = 8  // 14 = incubationTime + deathTime
    val immuneTime = 2 // 16 = incubationTime + deathTime + immuneTime
    val healTime = 2   // 18 = incubationTime + deathTime + immuneTime + healTime 

    // TODO: not implemented yet
    /*
    val airTraffic = false
    val airplaneProbability = 0.01
    val vipProbability = 0.05
    val reducedMobility = false
    */
  }

  import SimConfig._

  def waitingTimeBeforeMove: Int = randomBelow(5) + 1

  def isHealthyRoom(room: (Int, Int)): Boolean =
    !persons.exists((p: Person) => p.row == room._1 && p.col == room._2 && (p.sick || p.dead))

  val persons: List[Person] = for (id <- (1 to population).toList) yield new Person(id)
  
  class Person (val id: Int) {
    var infected = isInfectedInitially
    var sick = false
    var immune = false
    var dead = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def isCurrentRoomInfected: Boolean =
      persons.exists((p: Person) => p.row == this.row && p.col == this.col && p.infected)
    
    def getNeighbourRooms: List[(Int, Int)] = {
      List(
        ((row - 1 + roomRows) % roomRows, col),
        ((row + 1) % roomRows, col),
        (row, (col - 1 + roomColumns) % roomColumns),
        (row, (col + 1) % roomColumns))
    }
    
    def isInfectedInitially: Boolean = id <= (population * prevalenceRate).toInt
    
    def infectAction {
      infected = true
      afterDelay(incubationTime)(sickAction)
    }

    def sickAction {
      sick = true
      afterDelay(deathTime)(dieAction)
    }

    def dieAction {
      if (random < deathRate) {
        dead = true
      }
      afterDelay(immuneTime)(immuneAction)
    }

    def immuneAction {
      if (!dead) {
        // I assume a dead person cannot become immune
        sick = false
        immune = true
      }
      afterDelay(healTime)(healAction)
    }

    def healAction {
      if (!dead) {
        // I assume a dead person cannot become healthy
        immune = false
        infected = false
      }
    }

    def scheduleMove {
      afterDelay (waitingTimeBeforeMove) (move)
    }

    def nextMove {
      val possibleRooms = getNeighbourRooms.filter(isHealthyRoom)
      
      if (!possibleRooms.isEmpty) {
        val randomRoom = possibleRooms(randomBelow(possibleRooms.length))
        row = randomRoom._1
        col = randomRoom._2
      }
      // else stay in the current room
      
      if (!immune && !infected && random < transmissibilityRate && isCurrentRoomInfected) {
        // If he is already infected, we can skip this 
        // Possible infection only if:
        // - he is not immune
        // - he is in a room with at least 1 infected people
        // - random number is unlucky
        infectAction
      }
    }

    def move {
      if (!dead) {
        // dead people do not move
        nextMove

        // start again
        scheduleMove
      }
    }

    // persons initially infected will get sick 
    if (infected) {
      afterDelay(incubationTime)(sickAction)
    }
    
    // start simulation
    scheduleMove
    
  }
}
