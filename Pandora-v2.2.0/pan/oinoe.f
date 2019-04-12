      subroutine OINOE
     $(IMAGE,Z,DIDH,RUN,N,PLOT)
C
C     Rudolf Loeser, 1991 Aug 09
C---- Plots a run of dI/dh, for OSTUNI.
C     !DASH
      save
C     !DASH
      real*8 DIDH, RUN, Z, ZERO
      integer I, LINC, N
      character IMAGE*(*), PLOT*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external LOGO, LINK, HI, BYE
C
C               Z(N), DIDH(N), RUN(N)
      dimension Z(*), DIDH(*), RUN(*)
C
      call HI ('OINOE')
C     !BEG
      call LOGO   (DIDH, N, 1, ZL10SMA, RUN)
C
      LINC = 1
      do 101 I = 1,N
        call LINK (IMAGE, Z(I), RUN(I), PLOT, LINC)
  101 continue
C     !END
      call BYE ('OINOE')
C
      return
      end
