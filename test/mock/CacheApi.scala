package test.mock

import play.api.cache._
import play.api.mvc._

class MockCacheApi extends CacheApi {
     def get[T](key: String)(implicit evidence$2: scala.reflect.ClassTag[T]): Option[T] = ???

     def getOrElse[A](key: String,expiration: scala.concurrent.duration.Duration)(orElse: => A)(implicit evidence$1: scala.reflect.ClassTag[A]): A = orElse

     def remove(key: String): Unit = ???

     def set(key: String,value: Any,expiration: scala.concurrent.duration.Duration): Unit = ???
}
