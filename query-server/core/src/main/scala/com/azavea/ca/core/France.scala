package com.azavea.ca.core

import geotrellis.proj4._
import geotrellis.vector._
import geotrellis.vector.io._
import geotrellis.vector.io.json._

import spray.json._

case class FranceRegion(name: String, code: Int)

object FranceRegion {
  implicit object FranceRegionJsonReader extends JsonReader[FranceRegion] {
    def read(value: JsValue): FranceRegion =
      value.asJsObject.getFields("nom", "code") match {
        case Seq(JsString(name), JsNumber(code)) =>
          FranceRegion(name.replace(" ", "-"), code.toInt)
        case v =>
          throw new DeserializationException("FranceRegion expected, got $v")
      }
  }
}

object France {
  val paris = Point(2.3675537109375, 48.814098527355746)

  def buffer(p: Point, d: Double): Polygon =
    p.reproject(LatLng, WebMercator).buffer(d).reproject(WebMercator, LatLng)

  def parisBuffers: Map[String, Polygon] =
    Seq(5, 15, 25, 35, 45, 55, 65)
      .map { z =>
        val m = z * 10000
      (s"${m}-METERS", buffer(paris, m))
      }
      .toMap

  val regions: Vector[MultiPolygonFeature[FranceRegion]] = {
    val collection = Resource("france-regions.geojson").parseGeoJson[JsonFeatureCollection]
    (collection.getAllMultiPolygonFeatures[FranceRegion] ++ collection.getAllPolygonFeatures[FranceRegion].map(_.mapGeom(MultiPolygon(_))))
  }

  val regionsByName: Map[String, MultiPolygon] =
    regions
      .map { case Feature(geom, data) => (data.name, geom) }
      .toMap

  val geom = regions.map(_.geom).unionGeometries.as[MultiPolygon].get

  val boundingBox = geom.envelope
  val boundingBoxGeom = boundingBox.toPolygon

  object CQL {
    val inBoundingBox = CQLUtils.toBBOXquery("the_geom", boundingBox)
    val notInBoundingBox = s"DISJOINT(the_geom, ${boundingBoxGeom.toWKT})"

    val inFrance = s"INTERSECTS(the_geom, ${geom.toWKT})"
    val notInFrance = s"DISJOINT(the_geom, ${geom.toWKT})"

  }
}
