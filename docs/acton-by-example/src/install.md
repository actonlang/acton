# Installation

{{#tabs }}
{{#tab name="Debian / Ubuntu" }}
For Debian derivative distributions that use .dpkg and the APT ecosystem. Add the Acton APT repo and install from there:
```console
sudo install -m 0755 -d /etc/apt/keyrings
sudo wget -q -O /etc/apt/keyrings/acton.asc https://apt.acton-lang.io/acton.gpg
sudo chmod a+r /etc/apt/keyrings/acton.asc
echo "deb [signed-by=/etc/apt/keyrings/acton.asc] http://apt.acton-lang.io/ stable main" | sudo tee -a /etc/apt/sources.list.d/acton.list
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
