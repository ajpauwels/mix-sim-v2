docker volume create prometheus-data
docker stop $(docker ps -aqf "name=prometheus")
docker rm $(docker ps -aqf "name=prometheus")
docker run \
       --name prometheus \
       -p 9090:9090 \
       -v ./metrics/prometheus:/etc/prometheus \
       -v prometheus-data:/prometheus \
       --network metrics \
       --add-host=host.docker.internal:host-gateway \
       prom/prometheus
