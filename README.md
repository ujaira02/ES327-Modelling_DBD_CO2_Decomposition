# ES327 Individual Project - Modelling CO₂ Decomposition In A Surface-Confined Plasma Reactor
Plasma technology for CO₂ conversion is gaining traction due to its ability to operate under mild conditions, such as room temperature and atmospheric pressure, and effectively utilise intermittent renewable energy. Plasma, an ionised gas composed of electrons, ions, and both excited and neutral molecules, creates a reactive environment ideal for converting inert CO₂, particularly when confined to a surface by an applied electric field, which enhances density and reactivity. This project used Fortran-based scientific computing to develop a comprehensive model of CO₂ decomposition in a surface-confined plasma reactor, systematically investigating key design parameters such as electrode configuration, gas flow rates, and AC supply characteristics to capture transient reactive species and complex reaction kinetics. Findings indicate that optimising operational parameters, notably higher AC voltages, tailored frequency settings, and precise electrode geometry control, can significantly improve conversion rates and energy efficiency, though further advances in material durability and reactor scalability are needed. Overall, the model offers practical guidance for refining plasma-assisted CO₂ conversion processes and lays a robust foundation for sustainable industrial applications.

### Acknowledgements
Prof Evgeny Rebrov<br>
Laboratory for Energy Intensified Reactor Engineering<br>
School of Engineering<br>
The University of Warwick

Dr Nima Pourali<br>
School of Engineering<br>
The University of Warwick

Dr Pradeep Lamichhane<br>
School of Engineering<br>
The University of Warwick

### Installation
#### Fortran
Ensure that **gfortran** is installed on the machine.

```
gfortran --version
```
If it is not, follow the instructions on the Fortran website to do so: https://fortran-lang.org/learn/os_setup/install_gfortran

#### Program
Ensure that the following files are downloaded to the preferred working directory:
* **2103892_ES327_Code.f90**
* **CO2Modelling**
* **CO2ratesF.dat**
* **CO2ratesB.dat**
* **O2ratesF.dat**
* **O2ratesB.dat**

If downloading via **web browser**, **right-click** the file link and choose “**Save As...**” then select the desired folder.

### Setting Up
#### Open the Terminal
* On **macOS** or **Linux**, open the “**Terminal**” application.
* On **Windows**, open “**Command Prompt**”, “**PowerShell**”, or a terminal provided by the Fortran environment (such as **Git Bash** or the **MinGW** terminal).

#### Navigate to working directory

```
cd ~/ProjectDirectory
```

### Running the Program
#### Compile the Fortran code (Optional)

```
gfortran 2103892_ES327_Code .f90 -o CO2modelling
```

#### Run the Program

```
./CO2modelling
```

### Verify Output (Optional)
List the directory contents and find **plotdata.dat**.

```
ls
```

<br>
<br>
<br>

_To submit questions or queries, or to receive the model analysis code, please feel free to reach out directly._
