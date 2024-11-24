# Installation

Tip releases are built from the latest commit on the acton git repo main branch. They are built at least once a night, so can be thought of as nightlies but more up to date.

{{#tabs }}
{{#tab name="Debian / Ubuntu x86_64" }}
For Debian derivative distributions that use .dpkg and the APT ecosystem. Add the Acton APT tip repo and install from there:
```console
wget -q -O - https://apt.acton-lang.io/acton.gpg | sudo apt-key add -
echo "deb [arch=amd64] http://aptip.acton-lang.io/ tip main" | sudo tee /etc/apt/sources.list.d/acton.list
sudo apt-get update
sudo apt-get install -qy acton
```
{{#endtab }}
{{#tab name="Linux x86_64" }}
Copy and extract the dist tar ball. Download from https://github.com/actonlang/acton/releases/download/tip/acton-linux-x86_64-tip.tar.xz
```console
tar Jxf acton-linux-x86_64-tip.tar.xz
```
{{#endtab }}
{{#tab name="MacOS x86_64" }}
Copy and extract the dist tar ball. Download from https://github.com/actonlang/acton/releases/download/tip/acton-macos-x86_64-tip.tar.xz
```console
tar Jxf acton-macos-x86_64-tip.tar.xz
```
{{#endtab }}
{{#endtabs }}
