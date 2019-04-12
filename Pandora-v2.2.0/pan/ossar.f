      subroutine OSSAR
     $(IMAGE,DIDH,MYX,NW,LIST,L,Z,N,GOOD)
C
C     Rudolf Loeser, 1991 Aug 09
C---- Initializes plot of dI/dh, for OSTUNI.
C     !DASH
      save
C     !DASH
      real*8 DIDH, HNDRD, ONE, TEN, XL, XU, YL, YLL, YU, YUL, Z, ZERO
      integer IHI, ILO, L, LIST, MYX, N, NH, NV, NW
      logical GOOD
      character IMAGE*(*), NUMERO*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external DAMIS, DOBAR, ABOVED, BELOWD, KINIT, HI, BYE
C
C               DIDH(N,NW), MYX(NW), Z(N), LIST(L)
      dimension DIDH(*),    MYX(*),  Z(*), LIST(*)
C
      data HNDRD /1.D2/
      data NV,NH /55, 117/
C
      call HI ('OSSAR')
C     !BEG
C---- Horizontal axis limits
      call DAMIS  (DIDH, MYX, N, LIST, L, ILO, IHI)
      XL = Z(ILO)
      XU = Z(IHI)
C---- Vertival axis limits
      call DOBAR  (DIDH, MYX, N, NW, LIST, L, YU, YL)
      if(YU.le.ZERO) then
        YUL = ZERO
      else
        YUL = log10(YU)
      end if
      if(YL.le.ZERO) then
        YLL = YUL-TEN
      else
        YLL = log10(YL/HNDRD)
      end if
      call ABOVED (YUL, ONE, YUL)
      call BELOWD (YLL, ONE, YLL)
C---- Initialize plot
      call KINIT  (IMAGE, XL, XU, YLL, YUL, NV, NH, NUMERO, GOOD)
C     !END
      call BYE ('OSSAR')
C
      return
      end
