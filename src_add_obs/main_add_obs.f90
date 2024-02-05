!-----------------------------------------------------------------------
!                            CIMA UBA - CONICET                        !
!-----------------------------------------------------------------------
!BOP
!
! !PROGRAM:  main_add_obs
! 
! !INTERFACE:
!

program main_add_obs

! !USES:

   use vars_global, only: tb_con, nm_con, tablas, namelist_obs
  
   use convobs,  only: exec_conv
   

! 
! !DESCRIPTION: 
! \label{MODS:main_add_obs}
! 
!  Basado en el paquete bfge de IMPE 
!
!  Este es la rutina principal que permite agregar observaciones de 
!  estaciones automáticas a un archivo prepBUFR. 
! 
!  Subroutinas incluídas:
!  -m_vars_global.f90: módulo de declaración de variables globales
!    utilizadas en diversas subrutinas del programa;
!
!
!EOP			
!-----------------------------------------------------------------------
!
!
   implicit none

 
     write(*,*)   
     write(*,*)' ************************************'
     write(*,*)' ***   START add_obs program     *** ' 
     write(*,*)' ************************************'
     write(*,*) 
     
     write(*,*)
     write(*,*)' Reading the Namelist main.'
     write(*,*)

     open(2,file='add_obs.conf',status='unknown')

     read(2,tablas)
     read(2,namelist_obs)

     write(*,*)'Tablas: ',tb_con
     write(*,*)
     write(*,*)'Namelists: ',nm_con
 
! 1. Lectura/escritura de prepBUFR  

     call exec_conv()


end program main_add_obs



