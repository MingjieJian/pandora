      subroutine ZINT
     $(XT,INCX,FT,INCF,J,X,FM,FP,FL,F0)
C
C     Rudolf Loeser, 2007 Jan 08
C---- Computes test values, for CUBINT.
C     See (97 Jan 31)
C     !DASH
      save
C     !DASH
      real*8 DD, DM, DP, F0, FL, FM, FP, FT, X, XT
      integer IA, IB, IC, ID, INCF, INCX, J, JA, JB, JC, JD
C     !DASH
      external  DIVVY
      intrinsic abs
C
      dimension XT(*), FT(*)
C
C     !BEG
      IA = 1+INCX*(J-2)
      IB = IA+INCX
      IC = IB+INCX
      ID = IC+INCX
      JA = 1+INCF*(J-2)
      JB = JA+INCF
      JC = JB+INCF
      JD = JC+INCF
C
      call DIVVY ((X-XT(IB)), (XT(IB)-XT(IA)), DM)
      FM = FT(JB)+(FT(JB)-FT(JA))*DM
C
      call DIVVY ((XT(IC)-X), (XT(ID)-XT(IC)), DP)
      FP = FT(JC)-(FT(JD)-FT(JC))*DP
C
      DD = FT(JB)*(XT(IC)-X)+FT(JC)*(X-XT(IB))
      call DIVVY (DD, (XT(IC)-XT(IB)), FL)
C
      DP = abs(FP-FL)
      DM = abs(FM-FL)
      if(DP.lt.DM) then
        F0 = FP
      else
        F0 = FM
      end if
C     !END
C
      return
      end
