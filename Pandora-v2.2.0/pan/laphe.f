      subroutine LAPHE
     $(LU,IMAGE,NL,LAB)
C
C     Rudolf Loeser, 1998 Jun 22
C---- Prints plot for KANALE.
C     !DASH
      save
C     !DASH
      integer LU, NL
      character IMAGE*(*), LAB*(*)
C     !DASH
      external ABJECT, LINER, KPRINT, HI, BYE
C
      call HI ('LAPHE')
C     !BEG
      call ABJECT (LU)
      write (LU,100) LAB,NL
  100 format(' ','Plot 4: RGVL vs. ',A,'; (NL =',I4,')')
      call LINER  (1,LU)
C
      call KPRINT (IMAGE,LU)
C     !END
      call BYE ('LAPHE')
C
      return
      end
