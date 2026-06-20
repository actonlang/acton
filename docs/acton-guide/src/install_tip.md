# Installation

Tip releases are built from the latest commit on the acton git repo main branch. They are built at least once a night, so can be thought of as nightlies but more up to date.

{{#tabs }}
{{#tab name="Debian / Ubuntu" }}
For Debian derivative distributions that use .dpkg and the APT ecosystem. Add the Acton APT tip repo and install from there:
```console
sudo install -m 0755 -d /etc/apt/keyrings
sudo wget -q -O /etc/apt/keyrings/acton.asc https://apt.acton-lang.io/acton.gpg
sudo chmod a+r /etc/apt/keyrings/acton.asc
echo "deb [signed-by=/etc/apt/keyrings/acton.asc] http://aptip.acton-lang.io/ tip main" | sudo tee -a /etc/apt/sources.list.d/acton.list
sudo apt-get update
sudo apt-get install -qy acton
```
{{#endtab }}
{{#tab name="RPM / DNF / YUM" }}
For RPM distributions that use DNF or YUM. Add the Acton RPM tip repo and install from there:
```console
sudo rpm --import https://rpmtip.acton.now/acton.gpg
sudo tee /etc/yum.repos.d/acton-tip.repo >/dev/null <<'EOF'
[acton-tip]
name=Acton tip
baseurl=https://rpmtip.acton.now/$basearch
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://rpmtip.acton.now/acton.gpg
EOF
sudo dnf install -y acton
```
On systems that use YUM instead of DNF, use `sudo yum install -y acton`
for the final install command.
{{#endtab }}
{{#tab name="Linux x86_64" }}
Copy and extract the distribution tarball. Download from https://github.com/actonlang/acton/releases/download/tip/acton-linux-x86_64-tip.tar.xz
```console
tar Jxf acton-linux-x86_64-tip.tar.xz
```
{{#endtab }}
{{#tab name="Linux ARM64" }}
Copy and extract the distribution tarball. Download from https://github.com/actonlang/acton/releases/download/tip/acton-linux-aarch64-tip.tar.xz
```console
tar Jxf acton-linux-aarch64-tip.tar.xz
```
{{#endtab }}
{{#tab name="MacOS x86_64" }}
Copy and extract the distribution tarball. Download from https://github.com/actonlang/acton/releases/download/tip/acton-macos-x86_64-tip.tar.xz
```console
tar Jxf acton-macos-x86_64-tip.tar.xz
```
{{#endtab }}
{{#tab name="MacOS ARM64" }}
Copy and extract the distribution tarball. Download from https://github.com/actonlang/acton/releases/download/tip/acton-macos-aarch64-tip.tar.xz
```console
tar Jxf acton-macos-aarch64-tip.tar.xz
```
{{#endtab }}
{{#endtabs }}
