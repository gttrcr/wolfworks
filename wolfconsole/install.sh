#dotnet repo
wget https://packages.microsoft.com/config/ubuntu/21.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

#sdk
sudo apt-get update; \
  sudo apt-get install -y apt-transport-https && \
  sudo apt-get update && \
  sudo apt-get install -y dotnet-sdk-6.0


#start a kernel
lsp-wl repo name

#execute on kernel
/opt/Wolfram/WolframEngine/12.3/SystemFiles/Kernel/Binaries/Linux-ARM/WolframKernel -mathlink -LinkMode Connect -LinkProtocol TCPIP -LinkName 46747@127.0.0.1,56240@127.0.0.1 -LinkHost 127.0.0.1 -code '1+1'

#open ports
sudo lsof -i -P -n | grep -iF wolfram

