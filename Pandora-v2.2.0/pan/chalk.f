      subroutine CHALK
     $(XNE,N,ISTRK,IST,STE,JST,XNEST)
C
C     Rudolf Loeser, 1992 Feb 06
C---- Finds the NE-value used for the computation of Stark splitting of
C     Hydrogen lines. Also returns JST, depth-index of that NE-value.
C     (This is version 4 of CHALK.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, STE, XNE, XNEST
      integer IST, ISTRK, JST, N
C     !DASH
      external  HANUMAN, HI, BYE
      intrinsic abs
C
C               XNE(N)
      dimension XNE(*)
C
      data CRIT /1.D12/
C
      call HI ('CHALK')
C     !BEG
      if(IST.ne.0) then
        JST = abs(IST)
      else
        if((ISTRK.le.0).or.(ISTRK.gt.N)) then
          call HANUMAN (XNE,N,CRIT,ISTRK)
        end if
        JST = ISTRK
      end if
C
      IST = JST
      STE = XNE(JST)
C
      XNEST = STE
      if(IST.lt.0) then
        XNEST = -XNEST
      end if
C     !END
      call BYE ('CHALK')
C
      return
      end
