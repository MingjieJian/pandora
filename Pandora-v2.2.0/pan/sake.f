      subroutine SAKE
     $(KK,KINT)
C
C     Rudolf Loeser, 2005 Aug 09
C---- Sets up a frequency-index interval, so that no more than KMAX
C     are printed.
C     !DASH
      save
C     !DASH
      integer KINT, KK, KMAX
C     !DASH
      external HI, BYE
C
      data KMAX /32/
C
      call HI ('SAKE')
C     !BEG
      KINT = 1
C
  100 continue
        if((KK/KINT).le.KMAX) then
          goto 101
        end if
C
        KINT = 2*KINT
        goto 100
C
  101 continue
C     !END
      call BYE ('SAKE')
C
      return
      end
