      subroutine HIGRO
     $(NL,NSL,IQBDC)
C
C     Rudolf Loeser, 1988 Jul 21
C---- Check b-ratio option w.r.t. supplementary levels.
C     !DASH
      save
C     !DASH
      integer IQBDC, NL, NSL
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('HIGRO')
C     !BEG
      if(NSL.gt.NL) then
        if(IQBDC.gt.0) then
C
          write (MSSLIN(1),100)
  100     format('The option BDCALC may not be used when there are ',
     $           'supplementary levels.')
          call HALT ('HIGRO',1)
C
        end if
      end if
C     !END
      call BYE ('HIGRO')
C
      return
      end
