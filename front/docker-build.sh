docker build -t pds-front .
docker image tag pds-front st116vm101.rtb-lab.pl:443/pds-front
docker image push st116vm101.rtb-lab.pl:443/pds-front
