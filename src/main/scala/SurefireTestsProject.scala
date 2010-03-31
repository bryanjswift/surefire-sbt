package bryanjswift.testing

import java.io.File
import sbt._

trait SurefireTestsProject extends DefaultProject {
	// define path where surefire reports should live
	def surefireOutputPath = outputPath / "surefire-reports"
	// add SurefireReportListener to testListeners
	override def testListeners = {
		val surefireReportListener = new SurefireReportListener(surefireOutputPath.asFile)
		(surefireReportListener :: Nil) ++ super.testListeners
	}
	// action to clean reports
	lazy val cleanReports = task {
		val parentFolder = surefireOutputPath.asFile
		try {
			if (parentFolder.exists) { deleteDirectory(parentFolder) }
			parentFolder.mkdir
			None
		} catch {
			case e:Exception =>
				Some(e.getMessage)
		}
	}
	private def deleteDirectory(path:java.io.File):Boolean = {
		path.listFiles.foreach(f => if (f.isDirectory) deleteDirectory(f) else f.delete)
		path.delete
	}
	// make report cleaning happen before test execution
}
