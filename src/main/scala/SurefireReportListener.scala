package bryanjswift.testing

import java.io.{BufferedWriter,File,FileWriter,PrintWriter,StringWriter}
import java.util.Calendar
import org.scalatools.testing.{Event,Result => TestingResult}
import sbt._
import scala.xml.{Null,Text,UnprefixedAttribute => Attribute}

class SurefireReportListener(parentFolder:File) extends TestsListener {
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
	def doInit {
		if (!parentFolder.exists) parentFolder.mkdir
	}
  /** called once, at end. */
	def doComplete(finalResult: Result.Value) { }
}

object SurefireReportListener {
	def properties = {
		import scala.collection.jcl.Conversions.convertSet
		val properties = System.getProperties
		val keys = convertSet(properties.keySet)
		<properties>
			{for (key <- keys) yield <property /> % new Attribute("name", key.toString, new Attribute("value", properties.getProperty(key.toString), Null))}
		</properties>
	}
}

