#!/usr/bin/env bash

N=${1:-1}
M=${2:-1000000}

/spark/bin/spark-submit '--master=local[1]' \
   --conf 'spark.driver.memory=8G' \
   --class com.azavea.ca.synthetic.MesaPoke /mesa-jars/mesa-poke-assembly-0.jar \
   geomesa zookeeper root GisPwd geomesa.small_extents extent,${N},uniform:-180:180,uniform:-90:90,fixed:0,0.000000168:1:1:${M}

/spark/bin/spark-submit '--master=local[1]' \
   --conf 'spark.driver.memory=8G' \
   --class com.azavea.ca.synthetic.WavePoke /wave-jars/wave-poke-assembly-0.jar \
   geowave zookeeper root GisPwd geowave.small_extents space extent,${N},uniform:-180:180,uniform:-90:90,fixed:0,0.000000168:1:1:${M}
