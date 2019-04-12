      subroutine CHEMOR
     $(N,TE,BOT,TOP)
C
C     Rudolf Loeser, 2001 Nov 23
C---- Sets up ordinate limits for ADARE.
C     !DASH
      save
C     !DASH
      real*8 BOT, PW, TE, TOP, ZAX, ZIN, ZL
      integer IP, N
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external BELOWD, ABOVED, CHROME, HI, BYE
C
C               TE(N)
      dimension TE(*)
C
      call HI ('CHEMOR')
C     !BEG
      ZIN = +ZZLARGE
      ZAX = -ZZLARGE
      call CHROME (N, 1, TE, 1, ZZSMALL, ZAX, ZIN)
C
      ZL = log10(ZIN)
      IP = ZL
      PW = 10**IP
      call BELOWD (ZIN, PW, BOT)
C
      ZL = log10(ZAX)
      IP = ZL
      PW = 10**IP
      call ABOVED (ZAX, PW, TOP)
C     !END
      call BYE ('CHEMOR')
C
      return
      end
