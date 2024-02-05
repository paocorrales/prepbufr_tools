!-----------------------------------------------------------------------
!                           CIMA UBA-CONICET          !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  vars_global ---- Implements module global variables
! 
! !INTERFACE:
!
module vars_global

  implicit none
  !private
  
  save

! 
! !DESCRIPTION: 
! \label{MODS:vars_global}
! 
! Descripción:
! Este módulo corresponde a la declaración de variables globales que son
! utilizadas por las diversas rutinas del programa PREPBUFR_GEN.
!  
! !REVISION HISTORY: 
!
! 24Abr2019   P. Corrales   Adapatación inicial a partir de BFGE. 
!
!EOP			
!-----------------------------------------------------------------------

  
!vars main
!{ 
 character(150)  :: tb_con, tb_rad, tb_wmo, nm_con, nm_rad, nm_sat, nm_rog 
 
 namelist /tablas/tb_con, tb_rad, tb_wmo
 namelist /namelist_obs/nm_con, nm_rad, nm_sat, nm_rog 
!}
  
!vars m_config obs  
!{
  integer         :: data_prepbufr, data_bufr_r
  real            :: window
  character(150)  :: dir_salida_pb,  dir_salida_pb_r
  character(50)   :: new_prepbufr 
  character(250)  :: tdj_prepbufr
  character(10)   :: new_date_ana

  namelist /namelist_con/data_prepbufr, dir_salida_pb, new_prepbufr, &
                        new_date_ana, window 
  
  !namelist /namelist_rad/data_bufr_r, dir_salida_pb_r
  
  character(250)  :: usfilebufr  
!}

end module












