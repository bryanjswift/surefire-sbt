package bryanjswift.testing

import java.io.{BufferedWriter,File,FileWriter,PrintWriter,StringWriter}
import java.util.Calendar
import org.scalatools.testing.{Event,Result => TestingResult}
import sbt._
import scala.xml.{Null,Text,UnprefixedAttribute => Attribute}

class SurefireReportListener(parentFolder:File) extends TestsListener {
	if (parentFolder.exists) { SurefireReportListener.deleteDirectory(parentFolder) }
	parentFolder.mkdir
	private var suites = Map[String,TestSuite]()
	/** called for each class or equivalent grouping */
	def startGroup(name: String) {
		val suite = TestSuite(name)
		suites += (suite.clazz -> suite)
	}
	/** called for each test method or equivalent */
	def testEvent(event: TestEvent) {
		event.detail.foreach((detail) => {
			val suite = suites(TestSuite.suiteName(detail.testName))
			suites += (suite.clazz -> suite.addTest(detail))
		})
	}
	/** called if there was an error during test */
	def endGroup(name: String, t: Throwable) {
		endGroup(suites(name))
	}
	/** called if test completed */
	def endGroup(name: String, result: Result.Value) {
		endGroup(suites(name))
	}
	private def endGroup(suite:TestSuite) {
		val xml = suite.xml.toString
		val file = new File(parentFolder,"TEST-" + suite.clazz + ".xml")
		if (file.exists) { file.delete }
		val writer = new BufferedWriter(new FileWriter(file))
		writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
		writer.write(xml)
		writer.close
	}
	/** called once, at beginning. */
  def doInit { }
  /** called once, at end. */
  def doComplete(finalResult: Result.Value) { }
}

object SurefireReportListener {
	def deleteDirectory(path:java.io.File):Boolean = {
		path.listFiles.foreach(f => if (f.isDirectory) deleteDirectory(f) else f.delete)
		path.delete
	}
	def properties = {
		import scala.collection.jcl.Conversions.convertSet
		val properties = System.getProperties
		val keys = convertSet(properties.keySet)
		<properties>
			{for (key <- keys) yield <property /> % new Attribute("name", key.toString, new Attribute("value", properties.getProperty(key.toString), Null))}
		</properties>
	}
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
	def elapsed = (Calendar.getInstance.getTimeInMillis - startTime.getTimeInMillis) / 1000.0
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
			{SurefireReportListener.properties}
			{for (test <- testEvents) yield TestSuite.event2xml(test)}
		</testsuite> % (new Attribute("failures",testFailures.toString, new Attribute("errors",testErrors.toString,
										new Attribute("skipped",testSkips.toString, new Attribute("tests",testsRun.toString,
										new Attribute("name",clazz,Null))))))
	}
}

object TestSuite {
	def apply(clazz:String, testEvents:Set[Event], startTime:Calendar) =
		new TestSuite(clazz, testEvents, startTime)
	def apply(clazz:String) = new TestSuite(clazz, Set[Event](), Calendar.getInstance)
	def suiteName(name:String) = name.substring(0,name.lastIndexOf("."))
	def testName(name:String) = name.substring(name.lastIndexOf(".") + 1)
	private def event2xml(test:Event) = {
		<testcase>
			{
				test.result match {
					case TestingResult.Failure =>
						(<failure>{textStackTrace(test.error)}</failure>
							% (new Attribute("message",test.error.getMessage, new Attribute("type",test.error.getClass.getName, Null))))
					case TestingResult.Error =>
						(<error>{textStackTrace(test.error)}</error>
							% (new Attribute("message",test.error.getMessage, new Attribute("type",test.error.getClass.getName, Null))))
					case TestingResult.Skipped => <skipped></skipped>
					case _ => Text("")
				}
			}
		</testcase> % (new Attribute("classname",suiteName(test.testName), new Attribute("name",testName(test.testName),
									 new Attribute("time","0.000",Null))))
	}
	private def textStackTrace(t:Throwable) = {
		val stringWriter = new StringWriter
		val writer = new PrintWriter(stringWriter)
		t.printStackTrace(writer)
		writer.close
		Text(stringWriter.toString)
	}
}
