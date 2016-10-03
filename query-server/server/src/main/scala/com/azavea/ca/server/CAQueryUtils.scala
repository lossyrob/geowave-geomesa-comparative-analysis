package com.azavea.ca.server

import com.azavea.ca.server.geomesa.connection.GeoMesaConnection
import com.azavea.ca.server.geowave.GeoWaveQuerier
import com.azavea.ca.server.geowave.connection.GeoWaveConnection
import com.azavea.ca.server.results.{RunResult, TestResult}

import org.geotools.data.Query
import org.locationtech.geomesa.accumulo.index.QueryPlanner
import org.opengis.filter.Filter

trait CAQueryUtils {
  def gmTableName: String
  def gmFeatureTypeName: String

  def gwTableName: String
  def gwFeatureTypeName: String

  // Only use for server-side count aggregations.
  private var _geowaveQuerier: Option[GeoWaveQuerier] = None
  def geowaveQuerier: GeoWaveQuerier =
    _geowaveQuerier match {
      case Some(q) => q
      case None =>
        val q = GeoWaveQuerier(gwTableName, gwFeatureTypeName)
        _geowaveQuerier.synchronized {
          _geowaveQuerier = Some(q)
        }
        q
    }

  private var _geowaveDs: Option[org.geotools.data.DataStore] = None
  def geowaveDs =
    _geowaveDs match {
      case Some(ds) => ds
      case None =>
        val ds = GeoWaveConnection.geotoolsDataStore(gwTableName)
        _geowaveDs.synchronized {
          _geowaveDs = Some(ds)
        }
        ds
    }

  private var _geomesaDs: Option[org.locationtech.geomesa.accumulo.data.AccumuloDataStore] = None
  def geomesaDs =
    _geomesaDs match {
      case Some(ds) => ds
      case None =>
        val ds = GeoMesaConnection.dataStore(gmTableName)
        _geomesaDs.synchronized {
          _geomesaDs = Some(ds)
        }
          ds
    }

  def resetDataStores(): Unit = {
    _geowaveQuerier = None
    _geowaveDs = None
    _geomesaDs = None
  }

  def geowaveFeatureSource() = geowaveDs.getFeatureSource(gwFeatureTypeName)
  def geomesaFeatureSource() = geomesaDs.getFeatureSource(gmFeatureTypeName)

  def captureGeoWaveQuery(query: Filter): TestResult =
    TestResult.capture(GeoWaveConnection.clusterId, {
      geowaveFeatureSource().getFeatures(new Query(gwFeatureTypeName, query))
    })

  def captureGeoMesaQuery(query: Filter, loose: Boolean = false): TestResult = {
    val q = new Query(gmFeatureTypeName, query)
    q.getHints.put(org.locationtech.geomesa.accumulo.index.QueryHints.LOOSE_BBOX, loose)
    TestResult.capture(GeoMesaConnection.clusterId, {
      geomesaFeatureSource().getFeatures(q)
    })
  }

  def captureGeoMesaCountQuery(query: Filter, loose: Boolean = false): TestResult = {
    val q = new Query(gmFeatureTypeName, query)
    q.getHints.put(org.locationtech.geomesa.accumulo.index.QueryHints.EXACT_COUNT, true)
    q.getHints.put(org.locationtech.geomesa.accumulo.index.QueryHints.LOOSE_BBOX, loose)
    TestResult.capture(GeoMesaConnection.clusterId) { _ =>
      geomesaFeatureSource().getCount(q)
    }
  }
}
