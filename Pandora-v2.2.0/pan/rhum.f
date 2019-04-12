      subroutine RHUM
     $(N,NP,PL,SIG, BOT,TOP)
C
C     Rudolf Loeser, 1996 Mar 06
C---- Establishes ordinate limits, for SKYE.
C     !DASH
      save
C     !DASH
      real*8 BOT, ONE, PL, SIG, TOP, YLL, YUL
      integer J, N, NP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external CHROME, BELOWD, ABOVED, HI, BYE
C
C               PL(N,NP)
      dimension PL(N,*)
C
      call HI ('RHUM')
C     !BEG
      YUL = -ZZLARGE
      YLL = +ZZLARGE
      do 100 J = 1,NP
        call CHROME (N,1,PL(1,J), 1,SIG, YUL,YLL)
  100 continue
      call BELOWD   (YLL,ONE,BOT)
      call ABOVED   (YUL,ONE,TOP)
C     !END
      call BYE ('RHUM')
C
      return
      end
