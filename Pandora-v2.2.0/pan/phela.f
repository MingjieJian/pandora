      subroutine PHELA
     $(LU,IMAGE,NS,NL,LAB)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Prints plot for LAKANE.
C     !DASH
      save
C     !DASH
      integer LU, NL, NS
      character IMAGE*(*), LAB*(*)
C     !DASH
      external ABJECT, LINER, KPRINT, HI, BYE
C
      call HI ('PHELA')
C     !BEG
      call ABJECT (LU)
      write (LU,100) NS,NL,LAB
  100 format(' ','Plot 3: GNV-L, for L =',I3,' through L =',I3,
     $           ', vs. ',A)
      call LINER  (1,LU)
C
      call KPRINT (IMAGE, LU)
C     !END
      call BYE ('PHELA')
C
      return
      end
