docker exec -it flink-jobmanager flink run \
  -c basc.flink4traces.velocityreport.Main \
  /opt/flink/usrlib/flink-functions4traces-BioAmbients2Scala-assembly-1.0.jar \
  --topic <TOPIC> \
  --port 7324 \
  --bootstrap-servers kafka:29092 \
  --schema-registry http://schema-registry:8081 \
  --window-duration 10000 \
  --window-interval 2000 \
  --keep-past 10.0 \
  --purge-threshold 10000 \
  --microscopic-threshold 0.0001 \
  --zScore-threshold 3.0
