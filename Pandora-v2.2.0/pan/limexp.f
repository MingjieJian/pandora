      subroutine LIMEXP
     $(ARG,EARG)
C
C     Rudolf Loeser, 1991 Jan 08
C---- Computes an exponential for THALIA.
C     !DASH
      save
C     !DASH
      real*8 ARG, EARG
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
      call HI ('LIMEXP')
C     !BEG
      if(ARG.gt.ZLNLARG) then
        EARG = ZZLARGE
      else
        EARG = exp(ARG)
      end if
C     !END
      call BYE ('LIMEXP')
C
      return
      end
