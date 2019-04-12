      subroutine MARKD
     $(I,J,ANS,EQ,NE)
C     Rudolf Loeser, 1979 Apr 18
C     (Originally written for CDC 6400, 1975 Nov 24)
C---- If I .eq. J, sets ANS=EQ, otherwise, sets ANS=NE.
C     !DASH
      save
C     !DASH
      real*8 I, J
      character ANS*(*), EQ*(*), NE*(*)
C
C     !BEG
      ANS = NE
      if(I.eq.J) ANS = EQ
C     !END
C
      return
      end
