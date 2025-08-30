rm -rf /prometheus/01K*
promtool tsdb create-blocks-from openmetrics /etc/prometheus/metrics.out .
