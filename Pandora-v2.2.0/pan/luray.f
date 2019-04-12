      subroutine LURAY
     $(LU,IQUWT,WRTMN,WRTMX,YRATS)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Prints a message for PEACH.
C     !DASH
      save
C     !DASH
      real*8 WRTMN, WRTMX, YRATS
      integer IQUWT, LU
      character TIT*10
C     !DASH
      external LINER, HAKO, HI, BYE
C
      call HI ('LURAY')
C     !BEG
      if(LU.gt.0) then
        call LINER  (2, LU)
        if(IQUWT.gt.0) then
          write (LU,100) WRTMN,WRTMX
  100     format(' ','Option USEWTAB is on: the standard rates ',
     $               'integrations wavelengths table will be used,'/
     $           ' ','with wavelengths within the limits WRATMN =',
     $               1PE14.6,' and WRATMX =',E14.6,'.')
          call HAKO (YRATS, TIT)
          write (LU,101) TIT
  101     format(' ','For the calculations at these wavelengths, ',
     $               'YRATS = ',A,', (= METHOD).')
        else
          write (LU,102)
  102     format(' ','To use the standard rates integrations ',
     $               'wavelengths table, turn option USEWTAB on.')
        end if
      end if
C     !END
      call BYE ('LURAY')
C
      return
      end
