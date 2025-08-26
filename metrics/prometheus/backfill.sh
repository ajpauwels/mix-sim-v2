rm -rf /prometheus/01K*
promtool tsdb create-blocks-from openmetrics /etc/prometheus/metrics-2-48h.out .
