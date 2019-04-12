      subroutine VALE
     $(ZTM,K,ZL,ZH)
C
C     Rudolf Loeser, 1992 Dec 30
C---- Establishes ordinate limits, for BROOM.
C     (This is version 2 of VALE.)
C     !DASH
      save
C     !DASH
      real*8 TEN, ZH, ZL, ZTM
      integer IMAX, IMIN, K
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
      external MINMAXD, ABOVED, BELOWD, HI, BYE
C
C               ZTM(KM,4)
      dimension ZTM(*)
C
      call HI ('VALE')
C     !BEG
      call MINMAXD (ZTM,1,(4*K),IMIN,IMAX)
      call ABOVED  (ZTM(IMAX),TEN,ZL)
      call BELOWD  (ZTM(IMIN),TEN,ZH)
C     !END
      call BYE ('VALE')
C
      return
      end
