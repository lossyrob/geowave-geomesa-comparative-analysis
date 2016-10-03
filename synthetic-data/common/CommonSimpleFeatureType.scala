package com.azavea.ca.synthetic

import com.vividsolutions.jts.geom._
import org.geotools.feature.simple.SimpleFeatureTypeBuilder
import org.opengis.feature.simple.SimpleFeatureType
import org.geotools.referencing.CRS


object CommonSimpleFeatureType {

  val whoField = "who"
  val whatField = "what"
  val whenField = "when"
  val whereField = "where"
  val whyField = "why"

  // http://docs.geotools.org/latest/userguide/library/referencing/order.html
  val authorityFactory = CRS.getAuthorityFactory(true)
  val epsg4326 = authorityFactory.createCoordinateReferenceSystem("EPSG:4326")

  def apply(geometryType: String = ""): SimpleFeatureType = {
    val sftb = (new SimpleFeatureTypeBuilder).minOccurs(1).maxOccurs(1).nillable(false)

    sftb.setName(s"Common${geometryType}SimpleFeatureType")
    sftb.add(whoField, classOf[String])
    sftb.add(whatField, classOf[String])
    sftb.add(whenField, classOf[java.util.Date])
    sftb.setCRS(epsg4326)
    geometryType.toLowerCase match {
      case "point" => sftb.add(whereField, classOf[Point])
      case "line" => sftb.add(whereField, classOf[LineString])
      case "polygon" => sftb.add(whereField, classOf[Polygon])
      case "multipoint" => sftb.add(whereField, classOf[MultiPoint])
      case "multiline" => sftb.add(whereField, classOf[MultiLineString])
      case "multipolygon" => sftb.add(whereField, classOf[MultiPolygon])
      case _ => sftb.add(whereField, classOf[Geometry])
    }
    sftb.setDefaultGeometry(whereField)
    sftb.add(whyField, classOf[String])

    sftb.buildFeatureType
  }
}
