cd front
sh docker-build.sh
cd ..

cd scripts/mongo
sh docker-build.sh
cd ../..

scp docker-swarm.sh st116@st116vm101.rtb-lab.pl:~/pds
scp docker-compose.yml st116@st116vm101.rtb-lab.pl:~/pds
