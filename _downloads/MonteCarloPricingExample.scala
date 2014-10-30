/*
 * Copyright (c) 2011-2014 Haener Consulting. All rights reserved.
 */

package examples.finance.simulation

import examples.Example
import virtufin.finance.model._
import virtufin.util._
import virtufin.finance.scenario._
import virtufin.math.random.{MultivariateWienerIncrements, UncorrelatedStandardNormalRandomNumberGenerator, RandomGenerator}
import virtufin.finance.product._
import virtufin.finance.product.Currency._
import virtufin.finance.product.feature._
import virtufin.finance.simulation._
import virtufin.finance.simulation.EuropeanPlainVanillaOption._
import virtufin.finance.Time
import virtufin.simulation.{Message, AgentIdentifier}
import virtufin.finance.scenario.Price
import virtufin.finance.model.BlackParametersIdentifier
import virtufin.math.Correlation
import virtufin.simulation.Agents.AddAgent
import virtufin.finance.product.Stock
import virtufin.finance.product.Position
import virtufin.finance.product.PortfolioNode
import virtufin.finance.product.PositionNode
// implicit for converting to Simulatable
import virtufin.finance.simulation.PortfolioHierarchyAgent._
import virtufin.finance.model.ScenarioRequest
import virtufin.finance.product.EuropeanPlainVanillaOption
// implicit for Strategy and for converting to Simulatable
import virtufin.finance.product.EuropeanPlainVanillaOption._
import java.util.Calendar._
import akka.actor.{Props, ActorSystem}
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.SortedSet
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.iteratee.{Enumeratee, Iteratee, Enumerator}

/**
 * Example for Monte Carlo valuation on a portfolio level
 */
object MonteCarloPricingExample extends Example {
  // The trades: two EuropeanPLainVanilla

  val currency = USD

  val notional0 = Cash(100, currency)
  val cp0 = Call
  val strike0 = 180.0
  val underlying0 = Price(Stock("GS"), currency)
  val maturityDate0 = Day(2015, NOVEMBER, 18)
  val buyerPortfolio0 = "client0"
  val sellerPortfolio0 = "bank"
  val trade0 = EuropeanPlainVanillaOption(notional0, cp0, strike0, underlying0, maturityDate0, buyerPortfolio0, sellerPortfolio0)
  val id0 = "trade0"

  val notional1 = Cash(200, currency)
  val cp1 = Call
  val strike1 = 200.0
  val underlying1 = Price(Stock("IBM"), currency)
  val maturityDate1 = Day(2014, NOVEMBER, 18)
  val buyerPortfolio1 = "client1"
  val sellerPortfolio1 = "bank"
  val trade1 = EuropeanPlainVanillaOption(notional1, cp1, strike1, underlying1, maturityDate1, buyerPortfolio1, sellerPortfolio1)
  val id1 = "trade1"

  // Putting trades into PortfolioHierarchy
  val portfolioHierarchy = PortfolioHierarchy(
    PortfolioNode("root", List(
      PortfolioNode(buyerPortfolio0: PortfolioIdentifier,
        List(
          PositionNode(id0: PortfolioIdentifier, Position(1, trade0))
        )
      ),
      PortfolioNode(buyerPortfolio1: PortfolioIdentifier,
        List(
          PositionNode(id1: PortfolioIdentifier, Position(1, trade1))
        )
      )
    )
    )
  )
  implicit val portfolioHierarchyAgent: AgentIdentifier = "portfolioHierarchy"
  implicit val timeout = Timeout(10 seconds)

  // Observables and dates we need for model
  val trades = Seq((id0: PortfolioIdentifier) -> trade0, (id1: PortfolioIdentifier) -> trade1)
  val observables = trades.map(t => t._2.marketObservable)
  val fixings = trades.flatMap(t => t._2.fixings)
  val fixingDates = trades.flatMap(t => t._2.fixingDates)
  val settlementDates = trades.flatMap(t => t._2.settlementDates)
  val eventDates = SortedSet[Time](fixingDates ++ settlementDates: _*).toArray

  // We will put these into the Agents object
  val simulatables: Iterable[(AgentIdentifier, Simulatable)] = List(
    (id0: AgentIdentifier) -> (trade0: Simulatable),
    (id1: AgentIdentifier) -> (trade1: Simulatable),
    portfolioHierarchyAgent -> (portfolioHierarchy: Simulatable)
  )

  // The scenario at t0
  val x00 = 180.0
  val x01 = 200.0
  val t0 = Day(2013, NOVEMBER, 1)
  val scenario = Scenario(t0 -> Quotes(underlying0 -> x00, underlying1 -> x01))


  // Some implicits for default values

  import RandomGenerator.defaultRandomGenerator
  import UncorrelatedStandardNormalRandomNumberGenerator.defaultUncorrelatedStandardRandomNumberGenerator
  import MultivariateWienerIncrements.defaultMultivariateWienerIncrements
  import MultivariateBlackScholesScenarioModel.defaultParametersToMultivariateGeometricBrownianMotion

  // Setting up the Black-Scholes model for risk factors
  val model = MultivariateBlackScholesScenarioModel.apply
  val request = ScenarioRequest(observables, scenario, t0)

  // Black parameters
  val i0 = underlying0: BlackParametersIdentifier
  val mu0 = 0.02
  val sigma0 = 0.4
  val p0 = BlackParameters(mu0, sigma0)
  val i1 = underlying1: BlackParametersIdentifier
  val mu1 = -0.01
  val sigma1 = 0.3
  val p1 = BlackParameters(mu1, sigma1)

  val correlation = Correlation(underlying0, underlying1)
  val c = -0.3

  val mu = List(mu0, mu1)
  val b = LookupBuilder() += i0 -> p0 += i1 -> p1 += correlation -> c
  val parameters = ModelContext(b.build())
  val blackParameters = MultivariateBlackParameters.fromContext(observables, parameters).get

  implicit val modelDispatcher = ModelDispatcher()
  val context = MultivariateBlackScholesScenarioModel.modelContext(observables, blackParameters, eventDates)
  val numberScenarios = 10

  def scenarios = model.model(request, context).get.map(s => Enumerator.enumerate(s)).take(numberScenarios)

  def scenarioEnumerator = Enumerator.enumerate(scenarios)

  val system = ActorSystem("MonteCarloPricingExample")

  def createSimulation(system: ActorSystem, scenario: Enumerator[QuotesEvent], simulatables: Iterable[(AgentIdentifier, Simulatable)]): virtufin.finance.simulation.Simulation = {
    val agents = system.actorOf(Props(Agents))
    for {
      f <- Future.traverse(simulatables)(t => agents ? AddAgent(t._1, t._2))
    } yield f
    virtufin.finance.simulation.Simulation(scenario, agents)
  }

  // Create an Enumerator of Simulation objects
  def simulations = scenarioEnumerator.map(s => createSimulation(system, s, simulatables))

  // An Enumerator of an Enumerator of MessagesEvents
  def transactions = simulations.map(s => s.generateTransactions())

  // Transform that into an Enumerator of Lists of MessagesEvents
  val collectTransactions = Iteratee.fold(List[MessagesEvent]())((l: List[MessagesEvent], x: MessagesEvent) => l.::(x))

  // Create a Cashflows object if the Message is a Transfer of Cash and if either the source
  // or target Portfolio are as given in the argument. I.e. we filter out all the cashfrom from/to
  // a specific Porfolio
  def messageToCashflow(t: Day, sourceOrTargetPortfolio: PortfolioIdentifier): PartialFunction[Message, Pair[Day, Pair[Currency, Double]]] = {
    case Message(m: Transfer[_], id) if m.position.asset.isInstanceOf[Currency] &&
      (m.sourcePortfolio == sourceOrTargetPortfolio || m.targetPortfolio == sourceOrTargetPortfolio) =>
      if (m.sourcePortfolio == sourceOrTargetPortfolio) t ->(m.position.asset.asInstanceOf[Currency], m.position.amount)
      else t ->(m.position.asset.asInstanceOf[Currency], -1.0 * m.position.amount)
  }

  def messagesEventsToCashflows(messagesEvents: List[MessagesEvent], sourceOrTargetPortfolio: PortfolioIdentifier): Map[Currency, Cashflows] = {
    val x = messagesEvents.flatMap(e => e.messages.map(m => messageToCashflow(e.time, sourceOrTargetPortfolio)(m)))
    x.groupBy(a => a._2._1).map(b => (b._1, b._2.map(c => c._1 -> c._2._2))).map(d => (d._1, Cashflows(d._1, d._2)))
  }

  val sourceOrTargetPortfolio: PortfolioIdentifier = sellerPortfolio0

  // Apply now the transformations:
  def transactionsLists = transactions.map(t => t |>>> collectTransactions) &> Enumeratee.mapM(identity)

  // This gives an Enumerator mapping currencies to Cashflows objects
  def cashflows = transactionsLists.map(a => messagesEventsToCashflows(a, sourceOrTargetPortfolio))

  // Value the cashflows
  def value(cashflows: Map[Currency, Cashflows], modelDate: Day, discountCurves: Map[Currency, Double => Double]): Map[Currency, Cash] = {
    cashflows.map(c => c._1 -> CashflowsPricingModel.price(c._2, modelDate, discountCurves(c._1)))
  }

  val r = 0.02
  val discountCurves = Map[Currency, Double => Double](USD -> (t => Math.exp(-r*t)))

  def values = cashflows map (c => value(c, t0, discountCurves))

  // The price of the whole portfolio may now be obtained by aggregating the values
  // This is left as an exercise to the reader

  // Output
  transactionsLists |>>> Iteratee.foreach(output)
  Thread.sleep(2000)
  output("\nCashflows")
  cashflows |>>> Iteratee.foreach(output)
  Thread.sleep(2000)
  output("\nValues")
  values |>>> Iteratee.foreach(output)
  Thread.sleep(2000)
  system.shutdown()
  System.exit(0)
}
