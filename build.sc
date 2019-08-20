import mill._, scalalib._, scalafmt._
import ammonite.ops._
import $ivy.`com.lihaoyi::mill-contrib-bloop:0.5.0`

object checkout extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.0"

  object tests extends Tests {
    def ivyDeps        = Agg(ivy"com.lihaoyi::utest:0.7.1")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

