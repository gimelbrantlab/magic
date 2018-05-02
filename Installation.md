## Install git and R

### Ubuntu/Debian Linux

Run this command to install git on a Ubuntu/Debian Linux system:
```
sudo apt install git
```

To install R, run:
```
sudo echo "deb http://<my.favorite.cran.mirror>/bin/linux/ubuntu <my.ubuntu.version>/" | sudo tee -a /etc/apt/sources.list
/sources.list
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt update
sudo apt install r-base
```

To install the latest R version on Ubuntu 16.04 from CRAN cloud, the first line should be:
```
sudo echo "deb http://cloud.r-project.org/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
```
### Mac

And the same for Mac users. If you still don't have [brew](https://brew.sh/), please install it, it's what you want:
```
# /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
And now, install git and R: 
```
brew install git
brew install r
```

To update R: 
```
brew install r
```
## Install missing R packages

If automatic installation fails, you will need to install missing packages manually (see the full list [here](https://github.com/gimelbrantlab/magic/blob/master/Dependencies.md)). In general, command 
```
install.packages(package_name, dependencies = T)
```
should work. If you are working with libraries installed in a separate, non-default folder, add the path to this folder:
```
install.packages(package_name, dependencies = T, lib = PATH_TO_LIB)
```
