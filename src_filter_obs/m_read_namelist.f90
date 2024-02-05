!-----------------------------------------------------------------------
!                   CIMA UBA-CONICET                            !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  read_namelist
! 
! !INTERFACE:
!

module read_namelist

! !USES:

   use vars_global, only: tb_con, nm_con, new_date_ana, window, &
                          dir_salida_pb, new_prepbufr, tdj_prepbufr, &
                          namelist_con, usfilebufr

   use filter_obs, only: filter_obs_prepbufr
   
  
! 
! !DESCRIPTION: 
! \label{MODS:read_namelist}
! 
! Este módulo tiene como objetivo realizar a lectura del namelist. Esto incluye
! el listado de prepbufrs disponibles y variables como la fecha y hora del nuevo
! ciclo de asimilación y la ventana correspondiente. Lee tambien la ruta y
! nombre de salida del nuevo prepbufr.
!
! La información luego será usada por la rutina filter_obs donde se filtraran
! los registros de observación de acuerdo a si ocurrieron o no dentro de la
! nueva ventana de asimilación.
! 
!  Subroutinas incluídas:
!  -m_vars_global.f90: módulo de declaración de variables globales 
!     utilizadas en diversas subrutinas del programa;
!
!  -m_filter_obs.f90: módulo que calculará el nuevo DHR (diferencia entre la
!  hora de la observación y la del nuevo analisis) para cada observación y
!  conserva las que quedan dentro de la ventana de asimilación.  Escribe todo en
!  un nuevo prepbufr. 
!
!EOP			
!-----------------------------------------------------------------------
!
    
contains

  subroutine exec_conv()
        
    implicit none
          
    integer :: icount, contab, lp
        
    character(250), dimension(:), allocatable :: recfilebufr
      
    integer, parameter :: mxmn=35, mxlv=250
    real(8)            :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)
    character(8)       :: subset
    integer            :: idate

    icount = 1      
    contab = 0
       
    write(*,*)
    write(*,*)'[[[ EXECUTE CONVENTIONAL ]]]'
    write(*,*)
              
      !rutina lectura del namelist  
      !devuelve 2 parametros (cant, prepbufr_files)   
      call config_init_all(contab, recfilebufr) 
        
      !verifica que haya más de 0 archivos
         if (contab.ne.0) then
  
            !loop sobre los archivos 
            do lp=1,contab
    
                !archivo bufr
                usfilebufr=trim(recfilebufr(lp))
              
                write(*,*)
                write(*,*)'Archivo en proceso: ',lp,' de ',contab
                write(*,*)usfilebufr                          
              
      
                call filter_obs_prepbufr()
             
            icount = icount + 1
  
            end do

         else
            write(*,'(A)')'No hay prepbufrs para procesar!! '
            write(*,*)
            write(*,*)' >>>>>>>>>> '
    
         end if ! fin loop sobre prepbufrs

         deallocate(recfilebufr)
             

  end subroutine exec_conv

   
   subroutine config_init_all(epcount, filebufr)
   
    implicit none
    
    character(250), dimension(:), allocatable, intent(out) :: filebufr
     
    !linked list    
    type :: eplink  
      character(250) :: filecon  
      type(eplink), pointer :: next => null()  
    end type eplink
  
    type (eplink), pointer :: epraiz => null()
    type (eplink), pointer :: epdado => null()
  
    integer, intent(out) :: epcount
    integer :: ioerr, ik
  
           
     write(*,*)
     write(*,*)'Empezando a leer namelist PREPRW '   
     write(*,*)
    
     open(2,file=trim(nm_con),status='old')
     
     read(2,namelist_con)
    
     !directorio de salida y nombre del prepbufr
     tdj_prepbufr=trim(dir_salida_pb)//trim(new_prepbufr)
               
     epcount=0
     
     allocate(epraiz) ! Allocate begin of linked list
     epdado => epraiz ! point dado to begin of linked list
           
     !lectura dinamica
     do
      read (2,'(A150)',iostat=ioerr) epdado%filecon 

       if ( ioerr /= 0 ) exit

        epcount=epcount+1
    
        allocate(epdado%next)
        epdado => epdado%next   
     end do 

     allocate(filebufr(epcount))

     epdado => epraiz
     
     !adicionando vetores alocaveis
     do ik=1,epcount
 
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
      write(*,*)'::::: Parametros leidos :::::'
      write(*,*)      
      write(*,'(A14,2x,A10)')'New analysis: ',new_date_ana
      write(*,*)'Con ventana ',window,' horas.'
      write(*,*)'Prepbufr: ',trim(tdj_prepbufr)
      write(*,*)
      write(*,'(A19,2x,I4)')'Cantidad de bufrs: ',epcount                       
      write(*,*)'Archivos bufrs =>'
      write(*,*)filebufr(:)
      write(*,*)
         

          
   end subroutine config_init_all
   
end module     
  






