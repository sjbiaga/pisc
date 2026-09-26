docker exec -it flink-jobmanager flink run \
  -c basc.flink4traces.cdf.Main \
  /opt/flink/usrlib/flink-functions4traces-BioAmbients2Scala-assembly-1.0.jar \
  --topic <TOPIC> \
  --bootstrap-servers kafka:29092 \
  --schema-registry http://schema-registry:8081 \
  --allowed-lateness 5000 \
  --window-duration 60000 \
  --probability-threshold 0.5
