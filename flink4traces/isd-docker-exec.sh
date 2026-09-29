docker exec -it flink-jobmanager flink run \
  -c pisc.flink4traces.isd.Main \
  /opt/flink/usrlib/flink-functions4traces-StochasticPiCalculus2Scala-assembly-1.0.jar \
  --bootstrap-servers kafka:29092 \
  --schema-registry http://schema-registry:8081 \
  --window-duration 60000 \
  --window-interval 10000 \
  --allowed-lateness 5000 \
  --weight-blowout-threshold 2.0 \
  --port 7424 \
  --topic <TOPIC>
