docker exec -it flink-jobmanager flink run \
  /opt/flink/usrlib/flink-functions4traces-BioAmbients2Scala-assembly-1.0.jar \
  --topic <TOPIC> \
  --bootstrap-servers kafka:29092 \
  --schema-registry http://schema-registry:8081
