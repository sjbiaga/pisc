sbt 'beam4traces/runMain basc.beam4traces.isd.Main --runner=DirectRunner --streaming=true
  --kafka-bootstrap=localhost:9092
  --schema-registry=http://localhost:8081
  --opensearch-host=localhost
  --opensearch-port=9200
  --opensearch-user=admin
  --opensearch-pass=Admin!Password-123
  --window-duration=5
  --degeneracy-threshold=-5.0
  --blowout-threshold=4.6
  --kafka-topic=<TOPIC>
'
