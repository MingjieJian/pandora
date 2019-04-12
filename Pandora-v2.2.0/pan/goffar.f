      subroutine GOFFAR
     $(IVOIT,MTHEI,LU)
C
C     Rudolf Loeser, 1988 Nov 22
C---- Prints reminders about the numerical methods being used.
C     (This is version 4 of GOFFAR.)
C     !DASH
      save
C     !DASH
      integer IVOIT, LU, MTHEI
C     !DASH
      external LINER, HI, BYE
C
      call HI ('GOFFAR')
C     !BEG
      if(LU.gt.0) then
        call LINER (1,LU)
        if(IVOIT.eq.1) then
          write (LU,100) IVOIT
  100     format(' ','IVOIT = ',I2,T15,'The high-precision, slow-speed',
     $               ' Voigt function method (Rybicki) will be used.')
        else if(IVOIT.eq.2) then
          write (LU,101) IVOIT
  101     format(' ','IVOIT = ',I2,T15,'The medium-precision, medium-',
     $               'speed Voigt function method (Drayson) will be ',
     $               'used.')
        else if(IVOIT.eq.3) then
          write (LU,102) IVOIT
  102     format(' ','IVOIT = ',I2,T15,'The low-precision, high-speed ',
     $               'VOIGT function method (Peytremann) will be used.')
        else
          write (LU,103) 'IVOIT'
  103     format(' ','Tne value of ',A,' does not make sense.')
        end if
C
        call LINER (1,LU)
        if(MTHEI.eq.0) then
          write (LU,104) MTHEI
  104     format(' ','MTHEI = ',I2,T15,'The faster, less precise ',
     $               'Exponential Integral method (Cooley) will be ',
     $               'used.')
        else if(MTHEI.eq.1) then
          write (LU,105) MTHEI
  105     format(' ','MTHEI = ',I2,T15,'The slower, very precise ',
     $               'Exponential Integral method (Press) will be ',
     $               'used.')
        else
          write (LU,103) 'MTHEI'
        end if
      end if
C     !END
      call BYE ('GOFFAR')
C
      return
      end
