docker volume create grafana-data
docker stop $(docker ps -aqf "name=grafana")
docker rm $(docker ps -aqf "name=grafana")
docker run \
       --name grafana \
       -p 3000:3000 \
       --user "$(id -u)" \
       -v ./metrics/grafana:/etc/grafana \
       -v grafana-data:/var/lib/grafana \
       --network metrics \
       grafana/grafana
