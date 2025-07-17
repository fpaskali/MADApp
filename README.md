
## <img src="hex-MADApp.png" alt="drawing" width="200"/> Microarray Data Analysis Application (MADApp)
The repository of Microarray Data Analysis Application, an application for a quantification of microarray images.

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![Linux](https://img.shields.io/badge/Linux-FCC624?style=flat&logo=linux&logoColor=black)
[![Windows](https://badgen.net/badge/icon/windows?icon=windows&label)](https://microsoft.com/windows/)

## Installation
To install the required packages, load the project from RStudio and run the following command in the R console:
```{r}
renv::restore()
```
(Alternatively, you can open terminal in the MADApp root directory, start R and use the same command in R console.)

## System dependencies
Certain R packages require system libraries to be preinstalled. Below are the commands to install these libraries for different Linux distributions:

### Debian/Ubuntu

Use the following command to install the necessary dependencies:
```{bash}
sudo apt install libtiff-dev libfftw3-dev libcurl4-openssl-dev libssl-dev libxml2-dev
```

### Fedora/RHEL

Use this command to install the dependencies:
```{bash}
sudo dnf install libtiff-devel fftw-devel libcurl-devel openssl-devel libxml2-devel
```

> ⚠️ If you encounter errors in the installation process or additional dependencies are needed, please open an issue to report them.

## Start App
To start the application from RStudio, load the project and run the following command in the R console:
```{r}
shiny::runApp()
```

## AppImage
The AppImage version of MADApp can be downloaded [here](https://drive.google.com/file/d/1KJP6Bg_zwoOolSoX9CNFqNU1THlLA3Ge/view?usp=drive_link) and run without any installation.

Before using it, make sure the file has execution permission. The exectuion permission can be added by executing this command:
```bash
chmod +x MADApp-x86_64.AppImage
```
(Alternatively, execution permission can be enabled via the file's Properties dialog in a file manager.)

To start the AppImage version, either:
* Run it from a terminal:
```bash
./MADApp-x86_64.AppImage
```
* Or double-click the file in a file manager (after setting execution permissions).

> 💡 Running the AppImage via terminal is recommended to view any potential error messages.

### Compatibility
The AppImage has been tested on several popular Linux distributions, including Ubuntu, Debian, Fedora, openSUSE, CachyOS and Linux Mint. While it should work reliably across these systems, if you encounter any issues or unexpected behavior, please open an issue.

> ⚠️ For report generation to work, Pandoc must be installed separately.

## Preview

![tab1](preview/preview_1.png)
![tab2](preview/preview_2.png)
![tab3](preview/preview_3.png)
![tab4](preview/preview_4.png)
![tab5](preview/preview_5.png)
