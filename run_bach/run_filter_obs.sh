#!/bin/bash

# Rutina generar prepbufr con distintas ventanas de asimilación 

# Parámetros

FECHA_INI="20181117 19"         #YYYYMMDD HH
CICLOS=1                        #Ciclos de análisis a procesar
PATH_OUT=/home/paola.corrales/datosmunin3/DATA/OBS/dataout/prepbufr_nivel1/

for TIEMPO in $(seq -f "%02g" -1 $((10#$CICLOS-1)) )

do
	echo "En el paso " $TIEMPO " de " $CICLOS

	FECHA=$(date -d "$FECHA_INI + $(( ((10#$TIEMPO+1))*3600)) seconds" +"%Y%m%d%H")
        DIA=`echo $FECHA | cut -c1-8`
        HORA=`echo $FECHA | cut -c9-10`
        

	if [ "$HORA" -eq 0 ]; then

		FECHA0=$(date -d "$FECHA_INI + $(( ((10#$TIEMPO))*3600)) seconds" +"%Y%m%d%H")
	        DIA0=`echo $FECHA0 | cut -c1-8`
		PREP1=prepbufr.gdas.${DIA0}.t18z.nr.48h
                PREP2=prepbufr.gdas.${DIA}.t00z.nr.48h
 
        elif [ "$HORA" -gt 0 ] && [ "$HORA" -le 6 ]; then
		PREP1=prepbufr.gdas.${DIA}.t00z.nr.48h
		PREP2=prepbufr.gdas.${DIA}.t06z.nr.48h
        elif [ "$HORA" -gt 6 ] && [ "$HORA" -le 12 ]; then
		PREP1=prepbufr.gdas.${DIA}.t06z.nr.48h
		PREP2=prepbufr.gdas.${DIA}.t12z.nr.48h
	elif [ "$HORA" -gt 12 ] && [ "$HORA" -le 18 ]; then
                PREP1=prepbufr.gdas.${DIA}.t12z.nr.48h
                PREP2=prepbufr.gdas.${DIA}.t18z.nr.48h
	elif [ "$HORA" -gt 18 ]; then
 
		FECHA2=$(date -d "$FECHA_INI + $(( ((10#$TIEMPO+2)))) days" +"%Y%m%d%H")
                DIA2=`echo ${FECHA2} | cut -c1-8`
                PREP1=prepbufr.gdas.${DIA}.t18z.nr.48h
                PREP2=prepbufr.gdas.${DIA2}.t00z.nr.48h
	fi

	echo "*****************************************************"
	echo "Empiezo a procesar el tiempo" $FECHA
	echo "*****************************************************"

cat <<- EOF >namelist_conv.PREPRW
----------------- namelist_conv_PREPRW -----------------

&namelist_con
new_date_ana= $FECHA
dir_salida_pb= '${PATH_OUT}'
new_prepbufr= 'cimap.${DIA}.t${HORA}z.01h.prepbufr.nqc'
window= 1.0
/
/home/paola.corrales/datosmunin3/DATA/prepBUFR_20181122/${PREP1}
/home/paola.corrales/datosmunin3/DATA/prepBUFR_20181122/${PREP2}
------------
EOF

./filter_obs.x

done


