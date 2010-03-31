package bryanjswift.testing

import java.io.File
import sbt._

trait SurefireTestsProject extends DefaultProject {

	def surefireOutputDirectory =
		outputPath.asFile.getCanonicalPath + File.pathSeparator

	override def testListeners = {
		val surefireReportListener = new SurefireReportListener(surefireOutputDirectory)
		(surefireReportListener :: Nil) ++ super.testListeners
	}
}
