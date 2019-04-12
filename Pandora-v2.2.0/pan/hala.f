      subroutine HALA
     $(LODCG,NODCG,LIM,JQ,JX)
C
C     Rudolf Loeser, 1990 Apr 20
C---- Gets Z-limit for diffusion plots.
C     !DASH
      save
C     !DASH
      integer JQ, JX, LIM, LODCG, NODCG
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external  HI, BYE
      intrinsic min, max
C
      call HI ('HALA')
C     !BEG
      if(LODCG.lt.0) then
        JQ = 1
      else
        JQ = min(max(1,LODCG),LIM)
      end if
      if((JQ.lt.1).or.(JQ.gt.N)) then
        JQ = 1
      end if
C
      if(NODCG.lt.0) then
        JX = LIM
      else
        JX = min(max(1,NODCG),LIM)
      end if
      if((JX.lt.1).or.(JX.gt.N)) then
        JX = N
      end if
C
      if(JX.le.JQ) then
        JQ = 1
        JX = LIM
      end if
C     !END
      call BYE ('HALA')
C
      return
      end
