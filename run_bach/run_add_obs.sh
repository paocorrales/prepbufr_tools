#!/bin/bash

# Rutina para agregar observaciones a un prepbufr 

# Parámetros

FECHA_INI="20181117 19"         #YYYYMMDD HH
CICLOS=1                       #Ciclos de análisis a procesar
PATH_OUT=/home/paola.corrales/datosmunin3/DATA/OBS/dataout/prepbufr_nivel3/

for TIEMPO in $(seq -f "%02g" -1 $((10#$CICLOS-1)) )

do
	echo $TIEMPO

	FECHA=$(date -d "$FECHA_INI + $(( ((10#$TIEMPO+1))*3600)) seconds" +"%Y%m%d%H")
	FECHAI=$(date -d "$FECHA_INI + $((10#$TIEMPO*3600)) seconds" +"%Y%m%d%H")
	DIA=`echo $FECHA | cut -c1-8` 
	HORA=`echo $FECHA | cut -c9-10`
	echo "*****************************************************"
	echo "Empiezo a procesar el tiempo" $FECHA
	echo "*****************************************************"

cat <<- EOF >namelist_conv.PREPOBS
----------------- namelist_conv_PREPOBS -----------------

&namelist_con
data_prepbufr= $FECHA
dir_saida_pb= '${PATH_OUT}'
new_prepbufr= 'cimap.${DIA}.t${HORA}z.01h.prepbufr.nqc'
/
ADPSFC /home/paola.corrales/datosmunin3/DATA/OBS/datain/ADPSFC_n3/${FECHAI}.csv
ADPSFC /home/paola.corrales/datosmunin3/DATA/OBS/datain/ADPSFC_n3/${FECHA}.csv
------------
EOF

./add_obs.x

done


