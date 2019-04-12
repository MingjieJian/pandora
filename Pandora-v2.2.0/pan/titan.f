      subroutine TITAN
     $(ZIN,ZAX,BOT,TOP)
C
C     Rudolf Loeser, 1984 Jun 18
C---- Computes graph axis limits, log scale.
C     !DASH
      save
C     !DASH
      real*8 BOT, HALF, ONE, R, TOP, XHI1, XHI2, XLO1, XLO2, ZAX, ZAXL,
     $       ZERO, ZIN, ZINL
      logical ZAXOK, ZINOK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  BELOWD, ABOVED, HI, BYE
      intrinsic abs
C
      data XLO1,XLO2 /9.9D-1, 9.0D-1/
      data XHI1,XHI2 /1.01D0, 1.10D0/
C
      call HI ('TITAN')
C     !BEG
      ZINOK = ZIN.gt.ZERO
      ZAXOK = ZAX.gt.ZERO
      if(ZINOK) then
        ZINL = log10(ZIN)
      else
        ZINL = ZERO
      end if
      if(ZAXOK) then
        ZAXL = log10(ZAX)
      else
        ZAXL = ZERO
      end if
      call BELOWD     (ZINL, ONE, BOT)
      call ABOVED     (ZAXL, ONE, TOP)
C
      if(TOP.eq.ZERO) then
        if(ZIN.gt.XLO1) then
          BOT = log10(XLO1)
        else if(ZIN.gt.XLO2) then
          BOT = log10(XLO2)
        end if
      else if(BOT.eq.ZERO) then
        if(ZAX.lt.XHI1) then
          TOP = log10(XHI1)
        else if(ZAX.lt.XHI2) then
          TOP = log10(XHI2)
        end if
      else if(ZINOK.and.ZAXOK) then
        R = abs((ZAXL-ZINL)/(TOP-BOT))
        if(R.lt.HALF) then
          BOT = log10(XLO2*ZIN)
          TOP = log10(XHI2*ZAX)
        end if
      end if
C     !END
      call BYE ('TITAN')
C
      return
      end
