docker exec -it spark-master /opt/spark/bin/spark-submit \
  --class basc.spark4traces.isd.Main \
  --master spark://spark-master:7077 \
  /opt/spark/apps/spark-functions4traces-BioAmbients2Scala-assembly-1.0.jar \
  --kafka-bootstrap kafka:29092 \
  --schema-registry http://schema-registry:8081 \
  --opensearch-host opensearch \
  --opensearch-port 9200 \
  --opensearch-user admin \
  --opensearch-pass 'Admin!Password-123' \
  --watermark-interval 10 \
  --window-duration 5 \
  --degeneracy-threshold -5.0 \
  --blowout-threshold 4.6 \
  --kafka-topic <TOPIC>
