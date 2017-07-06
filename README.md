# Step Daemon #

Step Daemon (stepd) is an external planner for 3d printers that utilizes Marlin 
compatible firmware to allow direct step processing by an external computer and 
enables the use of complex pre-processing. By offloading the planning we are able 
to optimize the G-code pipeline so that you can reach maximum speed, with advanced 
features, on even the most complex shapes, without stutter or slowdowns. 
All this can be achieved with three simple pieces of hardware you probably 
already have: a Marlin compatible control board, a Raspberry Pi, and a USB cable.

Step Daemon utilizes mostly 64-bit double precision linear algebra and vector 
math from top to bottom, with some 32-bit single precision floating point used 
in hot spots where precision can be leveraged.

* Low RAM: a bit less than 200mb.
* Low CPU: runs at about 5-10% CPU on a Raspberry Pi 3.
* Multithreaded pipeline using Akka actors.
* Bicubic bed leveling with per-step accuracy (vs per-line).
* OctoPrint compatible.
* Developed alongside the direct stepper chunk support for Marlin.
* Works with Linux (including RPi and other ARM machines) and MacOS. Windows soon.

## Dependencies ##

* **socat** is required for the virtual serial port (PTY).
  * MacOS (Homebrew): *brew install socat*
  * Linux: *sudo apt-get install socat*
  * Windows: must do a manual build with Cygwin
* **Java JVM** for the desired machine.
  * MacOS: should be pre-installed
  * Raspbian: *sudo apt-get install oracle-java8-jdk*
  * Other Linux: Google around for java8 JDK installation instruction.
* **SBT** should come pre-bundled with stepd.

## Usage ##

* Build and run the server (from the base directory of the checkout):
```bash
./sbt clean server/assembly
java -jar server/target/scala-2.11/*.jar
```
* Pipe a gcode file directly to the server:
```bash
cat /tmp/pty-stepd-client &
cat hellbenchy.gcode | tee /tmp/pty-stepd-client
```
* Or connect OctoPrint directly to the server:
  * Add a custom serial port to OctoPrint for  */tmp/pty-stepd-client*.
  * Restart OctoPrint if port does not show in the list (make sure stepd is running).
  * Connect using *auto* baud rate (must be auto).

## Marlin Configuration ##
* Only XYZ builds currently supported (no core or delta support yet).
* Enable *CHUNK_SUPPORT*.
* (Optional) Enable *AUTO_BED_LEVELING_BILINEAR* for bed leveling (only supported mode currently).