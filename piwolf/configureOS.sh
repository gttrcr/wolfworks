#Update
sudo apt-get update -y
sudo apt-get upgrade -y
sudo apt-get install openssh-server unzip curl -y
sudo apt-get install sysstat -y
sudo apt-get install screen -y
sudo apt-get install build-essential cmake -y
sudo apt-get install uuid-dev -y

#Install wolfram
curl -LO https://wolfr.am/wolfram-engine-raspi-install
sudo chmod a+x wolfram-engine-raspi-install
sudo ./wolfram-engine-raspi-install

#Add wolfram libraries (not mandatory)
echo '/opt/Wolfram/WolframEngine/12.1/SystemFiles/Libraries/Linux-ARM' | sudo tee -a /etc/ld.so.conf
sudo ldconfig

sudo reboot
