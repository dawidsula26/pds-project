docker build -t pds-scripts-mongo .
docker image tag pds-scripts-mongo st116vm101.rtb-lab.pl:443/pds-scripts-mongo
docker image push st116vm101.rtb-lab.pl:443/pds-scripts-mongo
