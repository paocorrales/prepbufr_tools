!-----------------------------------------------------------------------
!         INPE/CPTEC, Development and Modeling Division, GDAD          !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  adpsfc_rw ---- Implements ADPSFC PrepBUFR read and write
! 
! !INTERFACE:
!
module adpsfc_rw

! !USES:

   use vars_global, only: tb_con, tdj_prepbufr, &
                          new_prepbufr, data_prepbufr, usfilebufr
   
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
!  Este modulo contiene una subrutina que permite leer prepbufrs 
!  serialmente y escribir un nuevo prepbufr con nuevos registros de observaciones
!  que obtiene a partir de los archivos tx dados.
!               
!  Links utiles: 
!  Mnemonics:
!  http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_1.htm
!  Report Types:
!  http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm
!
!
! !REVISION: 
!
!  24Apr2019   P. Corrales   Version inicial a partir de m_adpsfc de IMPE
!
!
!EOP                    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!

contains

 
   !read and write adpsfc
   subroutine adpsfc_rw_prepbufr() 
   
      implicit none
      
      !variaveis read
      real(kind = selected_real_kind(10,34)), dimension(5000,8)       :: matOBS 
      character*10, dimension(5000)    :: roesta
      character*14, dimension(5000)    :: timeobs       
      integer                          :: contobs, n
      
      !variaveis write   
      integer, parameter    :: vdmxmn=35, vdmxlv=1 !no caso de superficie max nivel = 1 
      character(80)         :: vdhdstr, vdobstr, vdqcstr, vdoestr
      real(8)               :: vdhdr(vdmxmn), vdobs(vdmxmn,vdmxlv),&
                               vdqcf(vdmxmn,vdmxlv),vdoer(vdmxmn,vdmxlv)

      character(8)          :: vdsubset, vdc_sid, col_sid
      character(10)         :: charwdate
      character(2)          :: cwmes,cwdia,cwhora,cwminute,cwsegundo,&
                               mm_prepb,dd_prepb,hh_prepb,mn_prepb
      character(4)          :: cwhhmm,cwano,yyyy_prepb, wmo_blk,wmo_sta
      
      character(20)         :: ccsid_st
      
      integer               :: anoi_prepb,mmi_prepb,ddi_prepb,hhi_prepb,nc,pp,nd,pq
      
      real                  :: difftime
      integer               :: vdunit_out, vdunit_table, vdidate,vdiret,countsb,jw
      character(1)          :: mwcomp,sattipo
      character(6)          :: subs 
   
      character(250)        :: prep_surf, esc_sfc,tb_ncep
      logical               :: file_exists
           
      real(8)               :: vdrstation_id, GrauRad,dir,vel
      equivalence(vdrstation_id,vdc_sid)

      integer, dimension(8) :: jdat,idat,it
      real,    dimension(5) :: rinc

      real                  :: deg2rad, latv, lonv
     
      real(8), parameter    :: missing_wrt=10.0e10
      integer               :: sqn, cnt_in, cnt_notin, cnt_fprs, &
                               cnt_felv, cnt_fdirv, cnt_fvvnt, cnt_ftmp, cnt_fpnmar, cnt_ftmpmar

      character(14)         :: time
      integer               :: cvano, cvmes, cvdia, cvhora, cvminute,cvsegundo, exsPB

      deg2rad = acos(-1.0)/180.0
           
      ! Abriendo un archivo de observaciones nuevas
    open(75,file=usfilebufr,status='unknown')

     contobs=0

!   Loop de lectura (declarado a matriz para suportar ate 5000 valores)
    do n=1,5000

        !encabezado de  ADPSFC
        !LAT    LON     ELV     Est  yyyymmddhhmmss  TOB        QOB
        !UOB    VOB     PRSS

        read(75,*,end=600)matOBS(n,1),matOBS(n,2),matOBS(n,3),roesta(n),timeobs(n), &
                          matOBS(n,4),matOBS(n,5),matOBS(n,6), matOBS(n,7),matOBS(n,8)
                          write(*,*)'Lectura de  ADPSFC: ',roesta(n),timeobs(n)
                          contobs = contobs+1
    end do
600 close(75)
        
    
    write(*,*)'Cantidad de observaciones ADPSFC: ',contobs     
                     
           
           
     !lista de mnemonics (ver em tabela NCEP)   
     !          1   2   3   4   5   6   7    8   9   10    11    12
      vdhdstr='SID XOB YOB DHR TYP ELV SAID T29 RPT  TCOR  SQN PROCN'
      vdobstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS  PMO'
      vdqcstr='PQM QQM TQM ZQM WQM NUL PWQ NUL NUL   PMQ'
      vdoestr='POE QOE TOE NUL WOE NUL PWE     '
      
      vdunit_out=30   
      vdunit_table=40 
      
     !table ncep
     tb_ncep=trim(tb_con)

     !nombre del PREPBUFR
     prep_surf=trim(tdj_prepbufr)
      
     write(*,*)
     write(*,*)'prepBUFR: ',prep_surf
     write(*,*)
      
     !verifica si prepbufr existe?
     inquire (file=prep_surf, exist=file_exists)

     if (file_exists) then
         exsPB=1
     else
         exsPB=0
     endif
           
     write(*,*)
     write(*,*)'*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*'
     write(*,'(A)')'Rutina adpsfc_write_prepbufr() - prepbufrs' 
     write(*,*)'*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*'
     write(*,*)
 
                
     !encode or append (0=encode (nao existe prepbufr)/ 1=append(ja
     !existe prepbufr))
       if(exsPB.eq.0)then
          write(*,*)' *.* ENCODE PREPBUFR *.* '
          open(vdunit_table,file=tb_ncep,action='read')
          open(vdunit_out,file=prep_surf,action='write',form='unformatted')
          call datelen(10)
          call openbf(vdunit_out,'OUT',vdunit_table)
       else
          write(*,*)' *.* APPEND PREPBUFR *.* '
          open(vdunit_table,file=tb_ncep)
          open(vdunit_out,file=prep_surf,status='old',form='unformatted')
          call openbf(vdunit_out,'IN',vdunit_out)
          call dxdump(vdunit_out,vdunit_table)
          call closbf(vdunit_out)
          open(vdunit_out,file=prep_surf,status='old',form='unformatted')
          call datelen(10)
          call openbf(vdunit_out,'APX',vdunit_table)   
       endif  


       vdidate=data_prepbufr   ! cycle time: YYYYMMDDHH
       vdsubset='ADPSFC'       ! Single level adpsfc data 
                 

       !open file prepbufr
       call openmb(vdunit_out,vdsubset,vdidate)
   
       sqn = 0
       
           
!     loop of subsets -- QOB,TOB
!     -----------------
      write(*,*)'Inicio loop para registros tipo: 181 (t,q)'

      do jw=1,contobs

! Separando la columna 2 (TEMPO) en 5 columnas (YYYY mm dd hh mm)
      
      time=timeobs(jw)
      
      cwano=time(1:4)
      cwmes=time(5:6)
      cwdia=time(7:8)
      cwhora=time(9:10)
      cwminute=time(11:12)
      cwsegundo=time(13:14)

      !convert string to int
      read(cwano,'(I4)')cvano       
      read(cwmes,'(I2)')cvmes
      read(cwdia,'(I2)')cvdia
      read(cwhora,'(I2)')cvhora
      read(cwminute,'(I2)')cvminute
      read(cwsegundo,'(I2)')cvsegundo
    
             
       charwdate=trim(cwano)//trim(cwmes)//trim(cwdia)//trim(cwhora)
       !horaminuto
       cwhhmm=trim(cwhora)//trim(cwminute)
       !tiempo en el nombre del prepbufr (refer.)
        yyyy_prepb=new_prepbufr(7:10)
        mm_prepb=new_prepbufr(11:12)
        dd_prepb=new_prepbufr(13:14)
        hh_prepb=new_prepbufr(17:18)
        
        !convert strint to int 
        read(yyyy_prepb,'(I4)')anoi_prepb
        read(mm_prepb,'(I2)')mmi_prepb
        read(dd_prepb,'(I2)')ddi_prepb        
        read(hh_prepb,'(I2)')hhi_prepb
                 
        !verificación para difftime
        idat(1) = anoi_prepb  ! yyyy (tiempo de analisis)
        idat(2) = mmi_prepb   ! mm
        idat(3) = ddi_prepb   ! dd
        idat(4) = 0           ! time zone
        idat(5) = hhi_prepb   ! hour
        idat(6) = 0           ! minute
        idat(7) = 0           ! second
        idat(8) = 0           ! millisecond

        jdat(1) = cvano       ! yyyy (tiempo de la observación)
        jdat(2) = cvmes       ! mm
        jdat(3) = cvdia       ! dd
        jdat(4) = 0           ! time zone
        jdat(5) = cvhora      ! hour
        jdat(6) = cvminute    ! minute
        jdat(7) = cvsegundo   ! second
        jdat(8) = 0           ! millisecond
        
        it=2 ! relativo al formato de tipo salida 
        
        !función de w3lib para calculo de difftime (DHR)
        call w3difdat(jdat,idat,it,rinc)
        
        difftime=rinc(2)
        

        !            
        !   Initialize vector with missing value
        !   ------------------------------------
           vdhdr=missing_wrt
           vdobs=missing_wrt
           vdqcf=missing_wrt
           vdoer=missing_wrt
        !
        !   !SID 
           col_sid=trim(roesta(jw))
        !   
           vdc_sid=trim(col_sid)
           vdhdr(1)=vdrstation_id             !SID: station identification
       
        
           if( matOBS(jw,2) < 0.0 )then               ! XOB: longitude
              vdhdr(2) = matOBS(jw,2) + 360.0
           else if( matOBS(jw,2) >= 360.0 )then
              vdhdr(2) = matOBS(jw,2) - 360.0
           else
              vdhdr(2) = matOBS(jw,2)
           end if
        
           vdhdr(3)=matOBS(jw,1)              ! YOB: latitude
           vdhdr(4)=difftime                  ! DHR: diff in time 
           vdhdr(6)=matOBS(jw,3)              ! ELV: station elevation
           vdhdr(9)=jdat(5)                   ! RPT: reported observation time
           vdhdr(10)=0                        ! TCOR: indicator whether obs.time in "dhr" was corrected 
           vdhdr(11)=sqn                      ! SQN: report sequence number
           vdhdr(12)=0                        ! PROCN: process number for thismpi run (obtained from script)


!          Mass reports
!          ------------          
           if ( matOBS(jw,8) < 2000000)then
               vdhdr(5)=181                    ! TYP:  report type (veja link 2 inicio codigo)
               vdobs(1,1)=matOBS(jw,8)/100     ! POB: pressure obs en hPa.
               vdobs(9,1)=matOBS(jw,8)         ! PRSS: surface pressure en Pa.
           else
               vdhdr(5)=187                    ! TYP:  report type (veja link 2 inicio codigo)
           end if

           vdobs(8,1)=0                    ! CAT: PrepBUFR data level category

           vdobs(2,1)=matOBS(jw,5)*1000000 ! QOB: specific humidity observation
           vdobs(3,1)=matOBS(jw,4)         ! TOB: temperature observation
           vdobs(4,1)=matOBS(jw,3)         ! ZOB: hight observation

           vdqcf(3,1)=2                    ! TWQ: temperature (quality) marker
           vdqcf(2,1)=2                    ! QWQ: spec. humidity (quality) marker
           vdqcf(1,1)=2                    ! PQM: presure obs (QUALITY) MARKER
           vdqcf(4,1)=2                    ! ZWQ: hight (quality) marker

           
           vdoer(3,1)=1                    ! TOE: temperature observation error
  
           call ufbint(vdunit_out,vdhdr,vdmxmn,1     ,vdiret,vdhdstr)
           call ufbint(vdunit_out,vdobs,vdmxmn,vdmxlv,vdiret,vdobstr)
           call ufbint(vdunit_out,vdoer,vdmxmn,vdmxlv,vdiret,vdoestr)
           call ufbint(vdunit_out,vdqcf,vdmxmn,vdmxlv,vdiret,vdqcstr)
           call writsb(vdunit_out)

                   
       end do               

!     loop of subsets -- UOB,VOB
!     -----------------
      write(*,*)'Inicio loop para registros tipo: 281 (u,v)'

      do jw=1,contobs

! Separando la columna 2 (TIEMPO) en 5 columnas (YYYY mm dd hh mm)
      
      time=timeobs(jw)
    
      cwano=time(1:4)
      cwmes=time(5:6)
      cwdia=time(7:8)
      cwhora=time(9:10)
      cwminute=time(11:12)
      cwsegundo=time(13:14)

      !convert string to int
      read(cwano,'(I4)')cvano       
      read(cwmes,'(I2)')cvmes
      read(cwdia,'(I2)')cvdia
      read(cwhora,'(I2)')cvhora
      read(cwminute,'(I2)')cvminute
      read(cwsegundo,'(I2)')cvsegundo
    
             
       charwdate=trim(cwano)//trim(cwmes)//trim(cwdia)//trim(cwhora)
       !horaminuto
       cwhhmm=trim(cwhora)//trim(cwminute)
       
       !tiempo en el nombre del prepbufr (refer.)
        yyyy_prepb=new_prepbufr(7:10)
        mm_prepb=new_prepbufr(11:12)
        dd_prepb=new_prepbufr(13:14)
        hh_prepb=new_prepbufr(17:18)
        
        !convert strint to int 
        read(yyyy_prepb,'(I4)')anoi_prepb
        read(mm_prepb,'(I2)')mmi_prepb
        read(dd_prepb,'(I2)')ddi_prepb        
        read(hh_prepb,'(I2)')hhi_prepb
                 
        !verificación para difftime
        idat(1) = anoi_prepb  ! yyyy (tiempo del analisis)
        idat(2) = mmi_prepb   ! mm
        idat(3) = ddi_prepb   ! dd
        idat(4) = 0           ! time zone
        idat(5) = hhi_prepb   ! hour
        idat(6) = 0           ! minute
        idat(7) = 0           ! second
        idat(8) = 0           ! millisecond

        jdat(1) = cvano       ! yyyy (tiempo de la  observación)
        jdat(2) = cvmes       ! mm
        jdat(3) = cvdia       ! dd
        jdat(4) = 0           ! time zone
        jdat(5) = cvhora      ! hour
        jdat(6) = cvminute    ! minute
        jdat(7) = cvsegundo   ! second
        jdat(8) = 0           ! millisecond
        
        it=2 ! relativo al formato de tipo de salida de intervalo
        
        !función de w3lib para calculo de difftime (DHR)
        call w3difdat(jdat,idat,it,rinc)
        
        difftime=rinc(2)
        

        !            
        !   Initialize vector with missing value
        !   ------------------------------------
           vdhdr=missing_wrt
           vdobs=missing_wrt
           vdqcf=missing_wrt
           vdoer=missing_wrt
        !
        !   !SID 
           col_sid=trim(roesta(jw))
        !   
           vdc_sid=trim(col_sid)
           vdhdr(1)=vdrstation_id             !SID: station identification

        
           if( matOBS(jw,2) < 0.0 )then               ! XOB: longitude
              vdhdr(2) = matOBS(jw,2) + 360.0
           else if( matOBS(jw,2) >= 360.0 )then
              vdhdr(2) = matOBS(jw,2) - 360.0
           else
              vdhdr(2) = matOBS(jw,2)
           end if
        
           vdhdr(3)=matOBS(jw,1)              ! YOB: latitude
           vdhdr(4)=difftime                  ! DHR: diff in time 
           vdhdr(6)=matOBS(jw,3)              ! ELV: station elevation
           vdhdr(9)=jdat(5)                   ! RPT: reported observation time
           vdhdr(10)=0                        ! TCOR: indicator whether obs.time in "dhr" was corrected 
           vdhdr(11)=sqn                      ! SQN: report sequence number
           vdhdr(12)=0                        ! PROCN: process number for thi mpi run (obtained from script)


!          Mass reports
!          ------------          
           if( matOBS(jw,8) < 2000000 )then
              vdhdr(5)=281                    ! TYP:  report type (veja link 2 inicio codigo)
              vdobs(9,1)=matOBS(jw,8)         ! PRSS: surface pressure en Pa
              vdobs(1,1)=matOBS(jw,8)/100     ! POB: presure obs en hPa
           else
              vdhdr(5)=287                    ! TYP:  report type (veja link 2 inicio codigo)
           end if

           vdobs(8,1)=6                    ! CAT: PrepBUFR data level category

           vdobs(5,1)=matOBS(jw,6)         ! UOB: zonal wind observation
           vdobs(6,1)=matOBS(jw,7)         ! VOB: meridian wind observation

           vdqcf(1,1)=2                    ! PQM: presure obs (QUALITY) MARKER
           vdqcf(5,1)=2                    ! WQM: U-, V-COMPONENT WIND (UOB/VOB) (QUALITY) MARKER

             
           call ufbint(vdunit_out,vdhdr,vdmxmn,1     ,vdiret,vdhdstr)
           call ufbint(vdunit_out,vdobs,vdmxmn,vdmxlv,vdiret,vdobstr)
           call ufbint(vdunit_out,vdoer,vdmxmn,vdmxlv,vdiret,vdoestr)
           call ufbint(vdunit_out,vdqcf,vdmxmn,vdmxlv,vdiret,vdqcstr)
           call writsb(vdunit_out)

                   
       end do


        call closmg(vdunit_out)
        call closbf(vdunit_out)
     
        close(vdunit_table)
        
      
        write(*,*)
        write(*,*)'*.*.*.*.*.* Diagnostico general *.*.*.*.*.*'
        write(*,*)
        write(*,*)'Total subsets:     2 x ',contobs
        write(*,*)
        write(*,*)'*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.'
            
        write(*,*)
        write(*,*)'*.* Fin de escritura prepbufrs: adpsfc *.*.*'
        write(*,*)
     
     
   end subroutine adpsfc_rw_prepbufr
     
   
end module
















