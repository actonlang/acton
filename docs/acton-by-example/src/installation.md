# Installation

{{#tabs }}
{{#tab name="Debian / Ubuntu" }}
For Debian derivative distributions that use .dpkg and the APT ecosystem. Add the Acton APT repo and install from there:
```console
wget -q -O - https://apt.acton-lang.io/acton.gpg | sudo apt-key add -
echo "deb [arch=amd64] http://apt.acton-lang.io/ stable main" | sudo tee /etc/apt/sources.list.d/acton.list
sudo apt-get update
sudo apt-get install -qy acton
```
{{#endtab }}
{{#tab name="MacOS" }}
```console
brew install actonlang/acton/acton
```
{{#endtab }}
{{#endtabs }}
