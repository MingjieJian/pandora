      subroutine OSSOLA
     $(ARRK,KNW,INDEX,KNT,ARRL)
C
C     Rudolf Loeser, 1984 Sep 12
C---- Extracts data to be plotted, for DULCIMR.
C     (This is version 2 of OSSOLA.)
C     !DASH
      save
C     !DASH
      real*8 ARRK, ARRL
      integer INDEX, J, JJ, KNT, KNW
C     !DASH
      external MOVE1, HI, BYE
C
C               ARRK(KNW,N), ARRL(KNW,KNT), INDEX(KNT)
      dimension ARRK(KNW,*), ARRL(KNW,*),   INDEX(*)
C
      call HI ('OSSOLA')
C     !BEG
      do 100 J = 1,KNT
        JJ = INDEX(J)
        call MOVE1 (ARRK(1,JJ),KNW,ARRL(1,J))
  100 continue
C     !END
      call BYE ('OSSOLA')
C
      return
      end
