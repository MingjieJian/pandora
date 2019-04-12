      subroutine CORD
     $(SUM,CU,MU,JU,CD,MD,JD)
C
C     Rudolf Loeser, 1978 Jul 26
C---- Chooses, for TAIFUN.
C     (MU and MD cannot both =0.)
C     !DASH
      save
C     !DASH
      real*8 CD, CU, SUM
      integer JD, JU, K, MD, MU
C     !DASH
      external HI, BYE
C
      call HI ('CORD')
C     !BEG
      if(MU.le.0) then
        K = 0
      else if(MD.le.0) then
        K = 1
      else if(CU.ge.CD) then
        K = 1
      else
        K = 0
      end if
C
      if(K.eq.0) then
        SUM = SUM+CD
        JU  = 0
        JD  = 1
      else
        SUM = SUM+CU
        JU  = 1
        JD  = 0
      end if
C     !END
      call BYE ('CORD')
C
      return
      end
