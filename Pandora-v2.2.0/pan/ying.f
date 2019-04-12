      subroutine YING
     $(NO,N,K,NDW,DL,SLF,Z,SL,ZL,FL,IQSLG)
C
C     Rudolf Loeser, 1992 Mar 20
C---- Plots, for PLOVER.
C     (This is version 3 of YING.)
C     !DASH
      save
C     !DASH
      real*8 BOT, DL, FL, ONE, SIG, SL, SLF, TOP, Z, ZL
      integer ID, IMAX, IMIN, IQSLG, IZ, K, N, NDW, NK, NO
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
C     !EJECT
      external ZOBEIDE, SIENA, YELLOW, PURPLE, ABOVED, BELOWD, JARED,
     $         MNMXD, LOGO, HI, BYE
C
C               DL(K), SLF(N,K), Z(N), SL(N,K), ZL(N), FL(max(N,K))
      dimension DL(*), SLF(*),   Z(*), SL(*),   ZL(*), FL(*)
C
      dimension IZ(5), ID(5)
C
      call HI ('YING')
C     !BEG
      if(NO.gt.0) then
        NK  = N*K
        SIG = ZL10SMA
C----   Compute logs
        call LOGO       (SLF, NK, 1, SIG, SL)
C----   Find ordinate limits
        call MNMXD      (SL, 1, NK, SIG, IMIN, IMAX)
        call BELOWD     (SL(IMIN), ONE, BOT)
        call ABOVED     (SL(IMAX), ONE, TOP)
C----   Plot all log(SLF) vs. Z
        call JARED      (NO, N, K, Z, ZL, SL, FL, SIG, BOT, TOP)
C
        if(IQSLG.gt.0) then
C----     Get DL-indices
          call ZOBEIDE  (DL, K, ID)
C----     Plot log(SLF) vs. Z
          call YELLOW   (NO, N, K, ID, DL, Z, ZL, SL, FL, SIG, BOT, TOP)
C----     Get Z-indices
          call SIENA    (NDW, N, IZ)
C----     Plot log(SLF) vs. DL
          call PURPLE   (NO, N, K, IZ, Z, DL, SL, FL, SIG, BOT, TOP)
        end if
      end if
C     !END
      call BYE ('YING')
C
      return
      end
