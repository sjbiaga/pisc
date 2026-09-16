docker exec -it flink-jobmanager flink run \
  -c pisc.flink4traces.sweepline.Main \
  /opt/flink/usrlib/flink-functions4traces-StochasticPiCalculus2Scala-assembly-1.0.jar \
  --topic <TOPIC> \
  --port 7224 \
  --bootstrap-servers kafka:29092 \
  --schema-registry http://schema-registry:8081 \
  --window-duration 5
