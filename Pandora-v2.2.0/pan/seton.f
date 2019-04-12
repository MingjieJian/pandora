      subroutine SETON
     $(J,NL,IJ,K)
C
C     Rudolf Loeser, 1998 Jun 23
C---- Delivers the next set of 10 transition index triples, for NOTES.
C     !DASH
      save
C     !DASH
      integer I, II, IJ, J, JJ, K, NL
C     !DASH
      external INDXIJ, HI, BYE
C
      dimension IJ(10,3)
C
      call HI ('SETON')
C     !BEG
      if(J.eq.1) then
        II = 0
        JJ = 1
      end if
C
      K = 0
  100 continue
      II = II+1
      if(II.gt.NL) then
        II = 0
        JJ = JJ+1
        if(JJ.le.NL) goto 100
      else
        if(II.ne.JJ) then
          K = K+1
          IJ(K,1) = II
          IJ(K,2) = JJ
        end if
        if(K.lt.10) goto 100
      end if
C
      do 101 I = 1,K
        call INDXIJ (IJ(I,1),IJ(I,2), IJ(I,3))
  101 continue
C     !END
      call BYE ('SETON')
C
      return
      end
