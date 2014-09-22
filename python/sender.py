import simplejson as json
import poster

poster.set_server_url("http://localhost:8080")

with open('alerts.json') as f:
    dashboards = json.load(f)

    for dashboard in dashboards:
        detector = poster.Detector(dashboard['title'], dashboard['description'])
        for alert in dashboard['alerts']:
            metric = poster.Metric(alert['title'], "", detector)
            poster.post_alert(detector, metric, alert, "", alert['date'])
