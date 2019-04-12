      subroutine PATH
     $(IMAGE,N,TL,F,FL,SYM)
C
C     Rudolf Loeser, 1997 Mar 25
C---- Enters a table into the plot image, for FENCE.
C     !DASH
      save
C     !DASH
      real*8 F, FL, SIG, TL
      integer N
      character IMAGE*(*), SYM*1
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external LOGO, SHRIMP, HI, BYE
C
C               TL(N), F(N), FL(N), SYM(1)
      dimension TL(*), F(*), FL(*), SYM(*)
C
      call HI ('PATH')
C     !BEG
      SIG = ZL10SMA
      call LOGO   (F, N, 1, SIG, FL)
      call SHRIMP (TL, N, 1, N, FL, 1, SYM, 1, SIG, 1, IMAGE)
C     !END
      call BYE ('PATH')
C
      return
      end
