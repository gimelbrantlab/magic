## Install git and R

### Ubuntu/Debian Linux

Run this command to install git if you don't have it yet, on a Ubuntu/Debian Linux machine:
```
sudo apt install git
```

To install R run:
```
sudo echo "deb http://<my.favorite.cran.mirror>/bin/linux/ubuntu <my.ubuntu.version>/" | sudo tee -a /etc/apt/sources.list
/sources.list
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo update
sudo apt-get install r-base
```

To install the latest R version on Ubuntu 16.04 from CRAN cloud, the first line would be:
```
sudo echo "deb http://cloud.r-project.org/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt
```
### Mac

And the same for Mac users. If you still don't have [brew](https://brew.sh/), please install it, it's what you want:
```
# /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
And now install git and R: 
```
brew install git
brew install r
```
