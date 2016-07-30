package simulations

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("prevalence rate") {
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01")
  }

  test("dead person stays dead") {
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val (row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("life cycle") {
    val es = new EpidemySimulator

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val infectedPerson = (es.persons.find { _.infected }).get

    //before incubation time
    while (es.agenda.head.time < incubationTime) {
      assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
      assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
      assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
      assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
      es.next
    }

    //incubation time has passed, there should be an event for getting sick
    assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
    while (es.agenda.head.time == incubationTime) { 
      es.next 
    }
    assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

    //wait for dieTime
    while (es.agenda.head.time < dieTime) {
      assert(infectedPerson.infected == true, "Sick person keeps infected")
      assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
      assert(infectedPerson.immune == false, "Sick person is not immune")
      assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
      es.next
    }

    assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
    while (es.agenda.head.time == dieTime) {
      es.next
    }
    
    while (es.agenda.head.time < immuneTime) {
      assert(infectedPerson.infected == true, "Sick person keeps infected before turning healed")
      assert(infectedPerson.sick == true, "Sick person keeps sick before immune action")
      assert(infectedPerson.immune == false, "Not immune yet")
      es.next
    }
    
    assert(es.agenda.head.time == immuneTime, "You should set a 'immune' event after 16 days")
    while (es.agenda.head.time == immuneTime) {
      es.next
    }
    
    while (es.agenda.head.time < healTime) {
      if (!infectedPerson.dead) {
        assert(infectedPerson.sick == false, "person is not sick anymore after immune")
        assert(infectedPerson.immune == true, "person is immune now")
      }
      es.next      
    }

    if (!infectedPerson.dead) {
      assert(es.agenda.head.time == healTime, "You should set a 'heal' event after 18 days")
    }
    
    while (es.agenda.head.time == healTime) { 
      es.next 
    }
    
    if (!infectedPerson.dead) {
    	assert(infectedPerson.infected == false, "Healed person is not infected after 18 days")
    	assert(infectedPerson.immune == false, "Healed person is not immune after 18 days")
    }
  }

  test("transmissibility rate") {
    var infectedTimes = 0
    for (i <- 0 to 100) {
      val es = new EpidemySimulator
      val healthyPerson = (es.persons find { p => !p.infected }).get
      es.persons.filter(p => p != healthyPerson) foreach { _.infected = true }

      while (es.agenda.head.time < 6) {
        es.next
      }

      infectedTimes = infectedTimes + (if (healthyPerson.infected) 1 else 0)
    }
    assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }

  test("cannot move to visibly infected room") {
    val es = new EpidemySimulator
    val randomPerson = es.persons(es.randomBelow(es.persons.size))
    var (row, col) = (randomPerson.row, randomPerson.col)
    val days = 200
    while (!es.agenda.isEmpty && es.agenda.head.time < days) {
      es.next
      if (row != randomPerson.row || col != randomPerson.col) {
        // person changed room
        val sickPersonsInRoom = es.persons filter { 
          p => p.row == randomPerson.row && p.col == randomPerson.col && p.id != randomPerson.id && p.sick 
        }
        assert(sickPersonsInRoom.size == 0, "Should be no sick persons in room moved to")
        row = randomPerson.row
        col = randomPerson.col
      }
    }
    
  }

}