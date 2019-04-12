      subroutine HIMBERT
     $(NKA,ZALBK,ALBK)
C
C     Rudolf Loeser, 1985 Dec 16
C---- Initializes ZALBK (for Kurucz opacities albedo).
C     (This is version 2 of HIMBERT.)
C     !DASH
      save
C     !DASH
      real*8 ALBK, ZALBK
      integer NKA
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external HI, BYE
C
C               ZALBK(NKA), ALBK(NKA)
      dimension ZALBK(*),   ALBK(*)
C
      call HI ('HIMBERT')
C     !BEG
      if(NKA.eq.2) then
        ZALBK(1) = ZZLARGE
        ZALBK(2) = ZZLARGE
        ALBK(1)  = ZZLARGE
        ALBK(2)  = ZZLARGE
      end if
C     !END
      call BYE ('HIMBERT')
C
      return
      end
