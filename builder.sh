#!/bin/bash

# Construimos el paquete con profiling
stack build --profile

# Creamos los directorios donde guardaremos los builds y las imagenes
mkdir -p buildlogs/
mkdir -p graphlogs/


# Nos movemos al directorio donde se guardaran todos los builds
stack exec -- counter-exe 11 +RTS -p -RTS +RTS -h -RTS
stack exec -- fair-exe 11 +RTS -p -RTS +RTS -h -RTS
stack exec -- interleave-exe 11 +RTS -p -RTS +RTS -h -RTS
stack exec -- shift-exe 11 +RTS -p -RTS +RTS -h -RTS

mv -f counter-exe.prof buildlogs/
mv -f fair-exe.prof buildlogs/
mv -f interleave-exe.prof buildlogs/
mv -f shift-exe.prof buildlogs/

pwd
pwd
cd graphlogs/
hp2ps -c -t  -s -b -m7 -y -e30in  ../counter-exe.hp
hp2ps -c -t  -s -b -m15 -y -e30in  ../fair-exe.hp
hp2ps -c -t  -s -b -m7 -y -e30in  ../interleave-exe.hp
hp2ps -c -t  -s -b -m7 -y -e30in  ../shift-exe.hp

rm counter-exe.aux
rm fair-exe.aux
rm interleave-exe.aux
rm shift-exe.aux

cd ../
rm counter-exe.*
rm fair-exe.*
rm interleave-exe.*
rm shift-exe.*
