# iacomus-alerts

A Clojure library designed to detect outliers in iacomus based dashboards and trigger
alerts to medusa.

Development and deployment
--------------------------

To start hacking on your local machine:
```bash
vagrant up
vagrant ssh
```

To deploy cerberus on AWS:
```
ansible-playbook ansible/deploy.yml -i ansible/inventory [ --private-key /path/to/mykey.pem ]
```

Note that the deployment requires [medusa](https://github.com/mozilla/medusa) to be deployed.

## Usage

Run `lein run` to launch the detector.
