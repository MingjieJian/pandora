      subroutine LANGUR
     $(KST,IHSSW,KHSSW)
C
C     Rudolf Loeser, 1992 Mar 16
C---- Sets KHSSW, the transition-dependent Stark convolution switch.
C     (This is version 2 of LANGUR.)
C     !DASH
      save
C     !DASH
      integer IHSSW, KHSSW, KST
C     !DASH
      external  HI, BYE
      intrinsic min, max
C
      call HI ('LANGUR')
C     !BEG
      if(IHSSW.le.0) then
        KST = 0
      end if
      KHSSW = KST
      KHSSW = min(max(KHSSW,0),1)
C     !END
      call BYE ('LANGUR')
C
      return
      end
