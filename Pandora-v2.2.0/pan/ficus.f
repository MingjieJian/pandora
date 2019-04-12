      subroutine FICUS
     $(F,X,T,RCNL,DELNL,ENN,ELL,H)
C
C     Rudolf Loeser, 1990 Nov 29
C---- Computes a value of F for upper-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 CON24, DELNL, EH, ELL, ENN, F, F1, FF, H, H1, ONE, P, RCNL,
     $       RR, RT, RTL, T, X
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  RIGEL, HI, BYE
      intrinsic min
C
      data F1,H1 /2.92D-1, 5.727D13/
C
      call HI ('FICUS')
C     !BEG
      call RIGEL (24, CON24)
      RT  = sqrt(T/CON24)
      RTL = log10(RT*X)
      FF  = (ONE-F1*RTL)**2
C
      if(ELL.eq.-ONE) then
        H  = ONE
        RR = ZZLARGE
      else
        RT = sqrt(T)
        H  = (H1*ENN*DELNL)/(X*RT)
        EH = exp(H)
        RR = RCNL/(ONE+EH)
      end if
C
      P = min(FF,RR)
      F = (X**3)*P
C     !END
      call BYE ('FICUS')
C
      return
      end
