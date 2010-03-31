package bryanjswift.testing

import java.io.File
import java.util.Calendar
import org.scalatools.testing.{Event,Result => TestingResult}
import sbt._
import scala.xml.{Null,UnprefixedAttribute => Attribute}

class SurefireReportListener(folder:String) extends TestsListener {
	val parentFolder = new File(folder)
	private var suites = Map[String,TestSuite]()
	/** called for each class or equivalent grouping */
	def startGroup(name: String) {
		val suite = TestSuite(name)
		suites += (suite.clazz -> suite)
		System.out.println("startGroup: " + suite.clazz)
	}
	/** called for each test method or equivalent */
	def testEvent(event: TestEvent) {
		event.detail.foreach((detail) => {
			val suite = suites(suiteName(detail.testName))
			suites += (suite.clazz -> suite.addTest(detail))
		})
		System.out.println("testEvent: " + event.detail.foldLeft("")(_ + " :: " + _.testName))
	}
	/** called if there was an error during test */
	def endGroup(name: String, t: Throwable) {
		System.out.println("endGroup with Error: " + name)
		endGroup(suites(name))
	}
	/** called if test completed */
	def endGroup(name: String, result: Result.Value) {
		System.out.println("endGroup: " + name)
		endGroup(suites(name))
	}
	private def endGroup(suite:TestSuite) {
		System.out.println(String.format("Tests run: %s, Failures: %s, Errors: %s, Skipped: %s", suite.testsRun.toString, suite.testFailures.toString, suite.testErrors.toString, suite.testSkips.toString))
	}
	/** called once, at beginning. */
  def doInit { }
  /** called once, at end. */
  def doComplete(finalResult: Result.Value) { }
	private def suiteName(testName:String) = testName.substring(0,testName.lastIndexOf("."))
}

class TestSuite(
	val clazz:String,
	val testEvents:Set[Event],
	val startTime:Calendar
) {
	lazy val testsRun = testEvents.filter(_.result != TestingResult.Skipped).size
	lazy val testFailures = testEvents.filter(_.result == TestingResult.Failure).size
	lazy val testErrors = testEvents.filter(_.result == TestingResult.Error).size
	lazy val testSkips = testEvents.filter(_.result == TestingResult.Skipped).size
	def addTest(testEvent:Event) = TestSuite(clazz, testEvents + testEvent, startTime)
	def elapsed = (Calendar.getInstance.getTimeInMillis - startTime.getTimeInMillis) / 1000
	private def canEqual(a:Any) = a.isInstanceOf[TestSuite]
	private def equals(t:TestSuite) = clazz == t.clazz && startTime == t.startTime
	override def equals(q:Any) =
		q match {
			case that:TestSuite =>
				canEqual(q) && equals(that)
			case _ =>
				false
		}
	override def hashCode = (41 * startTime.hashCode) + clazz.hashCode
	def xml = {
		<testsuite>
			{TestSuite.properties}
		</testsuite> % (new Attribute("failures",testFailures.toString, new Attribute("errors",testErrors.toString,
										new Attribute("skipped",testSkips.toString, new Attribute("tests",testsRun.toString,
										new Attribute("name",clazz,Null))))))
	}
}

object TestSuite {
	def apply(clazz:String, testEvents:Set[Event], startTime:Calendar) =
		new TestSuite(clazz, testEvents, startTime)
	def apply(clazz:String) = new TestSuite(clazz, Set[Event](), Calendar.getInstance)
	private def properties = {
		import scala.collection.jcl.Conversions.convertSet
		val properties = System.getProperties
		val keys = convertSet(properties.keySet)
		<properties>
			{for (key <- keys) yield <property /> % new Attribute("name", key.toString, new Attribute("value", properties.getProperty(key.toString), Null))}
		</properties>
	}
}
