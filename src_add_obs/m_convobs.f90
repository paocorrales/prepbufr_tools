!-----------------------------------------------------------------------
!                            CIMA UBA - CONICET                        !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  convobs ---- Implements convobs 
! 
! !INTERFACE:
!

module convobs

! !USES:

   use vars_global, only: tb_con, nm_con, data_prepbufr, &
                          dir_salida_pb, new_prepbufr, &
                          namelist_con, usfilebufr

   use adpsfc_rw
  
! 
! !DESCRIPTION: 
! \label{MODS:convobs}
! 
! Este módulo tiene como objetivo leer los archivos prepBUFR 
! listados en el namelist. exec_conv() ejecurará config_init_all(). 
!
!EOP			
!-----------------------------------------------------------------------
!
    
contains


  subroutine exec_conv()
        
    implicit none
          
    integer :: icount, imp, ipe, contab, lp
        
    character(250), dimension(:), allocatable :: recfilebufr
    character(6),   dimension(:), allocatable :: rectipobufr        
    
    icount = 1      
    contab = 0
       
    write(*,*)
    write(*,*)'[[[ Lee obs convencionales ]]]'
    write(*,*)
              
      !Rutina para leer el namelist 
      !devuelve 3 parametros (cont, tipobufr, qualbufr)   
      call config_init_all(contab, rectipobufr, recfilebufr) 
        
      !itera sobre los archivos hasta contab 
         if (contab.ne.0) then
  
            do lp=1,contab
    
!              imp = ceiling(float(lp)/float(npe))
!              ipe = ( ( lp + npe ) - npe * imp ) - 1
              
                !nombre del archivo bufr
                usfilebufr=trim(recfilebufr(lp))
              
                write(*,*)
                write(*,*)'Archivo en proceso: ',lp,' de ',contab-1
                write(*,*)usfilebufr                          
              
      
                  if(rectipobufr(lp).eq.'ADPSFC') then                
                     write(*,*)'Modulo ADPSFC'                 
                     call adpsfc_rw_prepbufr()

                  else

                     write(*,*)'No hay más archivos para procesar'
             
                  end if ! fin
              
             
            icount = icount + 1
  
            end do

         else
            write(*,'(A)')'No hay archivos para procesar'
            write(*,*)
            write(*,*)' >>>>>>>>>> '
    
         end if ! fin

         deallocate(rectipobufr)
         deallocate(recfilebufr)
             

  end subroutine exec_conv

   

   subroutine config_init_all(epcount, tipobufr, filebufr)
   
    implicit none
    
    character(250), dimension(:), allocatable, intent(out) :: filebufr
    character(6),   dimension(:), allocatable, intent(out) :: tipobufr
     
    !linked list    
    type :: eplink  
      character(6)   :: tipo_obs
      character(250) :: filecon  
      type(eplink), pointer :: next => null()  
    end type eplink
  
    type (eplink), pointer :: epraiz => null()
    type (eplink), pointer :: epdado => null()
  
    integer, intent(out) :: epcount
    integer :: ioerr, ik
  
           
     write(*,*)
     write(*,*)' Start routine CONV read namelist OBS '   
     write(*,*)
    
     open(2,file=trim(nm_con),status='old')
     
     read(2,namelist_con)
    
     !prepbufr
     tdj_prepbufr=trim(dir_salida_pb)//trim(new_prepbufr)
               
     epcount=0
     
     allocate(epraiz) ! Allocate begin of linked list
     epdado => epraiz ! point dado to begin of linked list
           
     !lectura dinamica
     do
      read (2,'(A6,1x,A150)',iostat=ioerr) epdado%tipo_obs, epdado%filecon 

       if ( ioerr /= 0 ) exit

        epcount=epcount+1
    
        allocate(epdado%next)
        epdado => epdado%next   
     end do 

     allocate(tipobufr(epcount))
     allocate(filebufr(epcount))

     epdado => epraiz
     
     do ik=1,epcount
 
      tipobufr(ik)= epdado%tipo_obs
      filebufr(ik)= epdado%filecon


       epdado=>epdado%next
     end do

         epdado => epraiz%next
         do 
            deallocate (epraiz)
            if(.not.associated(epdado)) exit
            epraiz => epdado
            epdado => epdado%next
         enddo
     
         
      write(*,*)
      write(*,*)'::::: Parametros leidos CONV :::::'
      write(*,*)      
      write(*,'(A6,2x,I10)')'Fecha: ',data_prepbufr
      write(*,*)'Prepbufr: ',trim(tdj_prepbufr)
      write(*,*)
      write(*,'(A21,2x,I4)')'Cantidad de archivos de obs: ',epcount-1                      
      write(*,*)'Archivos =>'
      write(*,*)filebufr(:)
      write(*,*)
         

          
   end subroutine config_init_all
   
end module     
  






