package bryanjswift.testing

import java.io.File
import sbt._

trait SurefireTestsProject extends DefaultProject {
	def surefireOutputPath = outputPath / "surefire-reports"
	override def testListeners = {
		val surefireReportListener = new SurefireReportListener(surefireOutputPath.asFile)
		(surefireReportListener :: Nil) ++ super.testListeners
	}
}
