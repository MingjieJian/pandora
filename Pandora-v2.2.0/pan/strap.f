      subroutine STRAP
     $(J,ITAU,TRMN,TRMX,TR1,RK1,TR2,RK2,TR,RKN,RK)
C
C     Rudolf Loeser, 1984 Apr 16
C---- Dumps for LUGGAGE.
C     !DASH
      save
C     !DASH
      real*8 RK, RK1, RK2, RKN, TR, TR1, TR2, TRMN, TRMX
      integer ITAU, J, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('STRAP')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100)
  100 format(' ',' Lev',3X,'i',8X,'TRMN',8X,'TRMX',9X,'TR1',9X,'RK1',9X,
     $           'TR2',9X,'RK2',10X,'TR',9X,'RKN',10X,'RK')
C
      call LINER (1, LUEO)
      write (LUEO,101) J,ITAU,TRMN,TRMX,TR1,RK1,TR2,RK2,TR,RKN,RK
  101 format(' ',2I4,1P9E12.4)
C     !END
      call BYE ('STRAP')
C
      return
      end
