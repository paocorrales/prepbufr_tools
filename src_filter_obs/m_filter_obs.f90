!-----------------------------------------------------------------------
!                         CIMA UBA-CONICET                             !
!-----------------------------------------------------------------------
!BOP
!
module filter_obs
! !USES:

   use vars_global, only: tb_con, nm_con, tablas, namelist_obs, &
                          new_date_ana, usfilebufr, window, &
                          new_prepbufr,tdj_prepbufr
! 
! !DESCRIPTION: 
! \label{MODS:filter_obs}
! 
! Basada en la estructura del Modulo m_adpsfc del paquete bfge
! 
!  Subroutinas incluidas:
!  -m_vars_global.f90: módulo de declaraciion de variables globales
!     utilizadas en diversas subrutinas del programa;
!
!  Descripcion:
!  Este modulo contiene una subrutina que permite leer varios prepbufrs 
!  serialmente y escribir un nuevo prepbufr con los registros de observaciones
!  dentro de determinada ventana de asimilación centrada en la hora del analisis
!               
!  Links uteis: 
!  Mnemonics:
!  http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_1.htm
!  Report Types:
!  http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm
!
!
! !REVISION: 
!
!  24Apr2019   P. Corrales   Version inicial a partir de m_adpsfc
!
!
!EOP                    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
contains 

 subroutine filter_obs_prepbufr()
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35,mxlv=200 
 !integer            :: mxlv
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8)               :: hdr(mxmn)
 real(8)               :: obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 character(250)        :: prep_out, tb_ncep 
 logical               :: file_exists
 integer               :: exsPB,unit_in,unit_out,unit_table

 INTEGER               :: ireadmg,ireadsb

 character(8)          :: subset
 integer               :: idate,ndate

 integer               :: nmsg,ntb,imk,iret,nlev
 character(8)          :: c_sid
 real(8)               :: rstation_id,new_dhr
 equivalence(rstation_id,c_sid)

 integer               :: nano,nmes,ndia,nhora, &
                          oano,omes,odia,ohora
 character(10)         :: old_date_ana 
 real, dimension(5)    :: dif_ana
 integer, dimension(8) :: jdat,idat,it
 real                  :: half_window


 !Variables para abrir/escribit archivos
 !-------------------------------------------------------------------------
 
 unit_in=20
 unit_out=30
 unit_table=40

 tb_ncep=trim(tb_con)           !tabla ncep
 prep_out=trim(tdj_prepbufr)    !nombre del nuevo PREPBUFR

 write(*,*)
 write(*,*)'Nuevo prepbufr: ',prep_out
 write(*,*)

 !verifica si el  prepbufr existe
 inquire (file=prep_out, exist=file_exists)

 if (file_exists) then
     exsPB=1
 else
     exsPB=0
 endif

 !encode or append (0=encode (no existe prepbufr)/ 1=append(existe
 !prepbufr))
     if(exsPB.eq.0)then
        write(*,*)'*.*.* ENCODE PREPBUFR *.*.*'
        open(unit_table,file=tb_ncep) !,action='read')
        open(unit_out,file=prep_out,action='write',form='unformatted')
        call datelen(10)
        call openbf(unit_out,'OUT',unit_table)
     else
        write(*,*)'*.*.* APPEND PREPBUFR *.*.*'
        open(unit_table,file=tb_ncep)
        open(unit_out,file=prep_out,status='old',form='unformatted')
        call openbf(unit_out,'IN',unit_out)
        call dxdump(unit_out,unit_table)
        call closbf(unit_out)
        open(unit_out,file=prep_out,status='old',form='unformatted')
        call datelen(10)
        call openbf(unit_out,'APN',unit_table)
     endif
 
rewind(unit_table) 

 !Prepara fecha de nuevo analisis
 !-------------------------------------------------------------------------

 read(new_date_ana(1:4),'(I4)')nano
 read(new_date_ana(5:6),'(I2)')nmes
 read(new_date_ana(7:8),'(I2)')ndia
 read(new_date_ana(9:10),'(I2)')nhora

 jdat(1) = nano       ! yyyy (new analysis)
 jdat(2) = nmes       ! mm
 jdat(3) = ndia       ! dd
 jdat(4) = 0          ! time zone
 jdat(5) = nhora      ! hour
 jdat(6) = 0          ! minute
 jdat(7) = 0          ! second
 jdat(8) = 0          ! millisecond

 read(new_date_ana(1:10),'(I10)')ndate
 write(*,*) 'Nuevo analisis',ndate
! Abre prepbufrs viejos y lee los registro
!--------------------------------------------------------------------------

 !verifica si el  prepbufr existe
 inquire (file=usfilebufr, exist=file_exists)

 if (file_exists) then
 
    open(unit_in,file=usfilebufr,form='unformatted',status='old')
    call openbf(unit_in,'IN',unit_in)
    call dxdump(unit_in,unit_table)
    call datelen(10)
      nmsg=0
      msg_report: do while (ireadmg(unit_in,subset,idate) == 0) !old prepbufr
      call openmb(unit_out,subset,ndate)                        !new prepbufr
       
! Defino tamñano de las matrices
!-------------------------------------------------------------------------

        nmsg=nmsg+1
        ntb = 0
        sb_report: do while (ireadsb(unit_in) == 0)             !old prepbufr
        
          ntb = ntb+1
          call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
          call ufbint(unit_in,obs,mxmn,mxlv,iret,obstr)
          call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
          call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)
       
          write(old_date_ana,'(I10)') idate
          !convert string to int
          read(old_date_ana(1:4),'(I4)')oano
          read(old_date_ana(5:6),'(I2)')omes
          read(old_date_ana(7:8),'(I2)')odia
          read(old_date_ana(9:10),'(I2)')ohora

          idat(1) = oano       ! yyyy (old analysis)
          idat(2) = omes       ! mm
          idat(3) = odia       ! dd
          idat(4) = 0          ! time zone
          idat(5) = ohora      ! hour
          idat(6) = 0          ! minute
          idat(7) = 0          ! second
          idat(8) = 0          ! millisecond

          it=2

          call w3difdat(jdat,idat,it,dif_ana)
          new_dhr=hdr(4)-dif_ana(2)
          hdr(4)=new_dhr

 !Reviso si la observación está dentro de la ventana
 !-------------------------------------------------------------------- 

          half_window = window/2.0 
          if (abs(new_dhr) <= half_window) then
          !Escribo el reporte en el nuevo prepbufr 
  
           nlev = iret
           call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
           call ufbint(unit_out,obs,mxmn,nlev,iret,obstr)
           call ufbint(unit_out,oer,mxmn,nlev,iret,oestr)
           call ufbint(unit_out,qcf,mxmn,nlev,iret,qcstr)
           call writsb(unit_out)

          else
            cycle
          endif
        enddo sb_report             !old prepbufr
        call closmg(unit_out)       !new prepbufr
      enddo msg_report              !old prepbufr
    call closbf(unit_in)
    call closbf(unit_out)          
    close(unit_table)

 endif
 
 end subroutine filter_obs_prepbufr

end module
