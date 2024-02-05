!-----------------------------------------------------------------------
!                           CIMA UBA-CONICET                           !
!-----------------------------------------------------------------------
!BOP
!
! !PROGRAM:  prepbufr_gen 
! 
! !INTERFACE:
!

program prepbufr_generation

! !USES:

   use vars_global, only: tb_con, nm_con, tablas, namelist_obs
  
   use read_namelist,  only: exec_conv
   

! 
! !DESCRIPTION: 
! \label{MODS:prepbufr_gen}
! 
!  Este es el programa principal de PREPBUFR_GEN que ejecuta todos los modulos
!  subsiguientes. Esttá basado en los programa de INPE-CPTEC para la
!  maniputación de observaciones, en particular BFGE.f90. Permite leer los archivos prepbufr listados
!  en el namelist y para cada registro de observación calcular un nuevo valor
!  para DHR (diferencia temporal entre el tiempo de la observación y el tiempo
!  del analisis) de acuerdo a un nuevo tiempo de analisis (nuevo ciclo).
!  Finalmente guarda en un nuevo prepbufr las observaciones que ocurrieron
!  dentro de la ventana de asimilacion (centrada) tambien definida en el
!  namelist.
!
!  Por ahora esto solo maneja observaciones convenciones.
! 
!  Subroutinas incluídas:
!  -m_vars_global.f90: módulo de declacion de variables globales
!    utilizadas en diversas subrotinasd del programa.
!  -m_read_namelist.f90 módulo de lectura del namelist, listado de los archivos
!  prepbufr disponibles y algunas variables de interés.
!
!  24Apr2019   P. Corrales   Verión inicial.
!
!EOP			
!-----------------------------------------------------------------------
!
!
   implicit none

 
     write(*,*)   
     write(*,*)' ************************************'
     write(*,*)' ***   START prepbufr_gen.x      *** ' 
     write(*,*)' ************************************'
     write(*,*) 
     
     write(*,*)
     write(*,*)'Leyendo la confifuración global'
     write(*,*)

     open(2,file='prepbufr_gen.conf',status='unknown')

     read(2,tablas)
     read(2,namelist_obs)

     write(*,*)'Tablas: ',tb_con
     write(*,*)
     write(*,*)'Namelists: ',nm_con
 

     call exec_conv()


end program prepbufr_generation



