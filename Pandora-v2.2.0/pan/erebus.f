      subroutine EREBUS
     $(NO,N,Z,TE,XNE,HND,ZT,DTE,R1N,IQPZT,W)
C
C     Rudolf Loeser, 2001 Nov 20
C---- Optional supplementary input printout.
C     (This is version 4 of EREBUS.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, DTE, HND, R1N, TE, W, XNE, Z, ZT
      integer ILNE, ILNH, IN, IQPZT, IS, IVEC, MOX, N, NO
      logical ISO
C     !DASH
      external PRIAM, CONSTD, BESURE, RUBEES, ADARE, SABINE, HALLETT,
     $         WGIVE, LINER, HI, BYE
C
      dimension W(*)
C
C               Z(N), TE(N), XNE(N), HND(N), ZT(N), DTE(N)
      dimension Z(*), TE(*), XNE(*), HND(*), ZT(*), DTE(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ILNE  ),(IN( 2),ILNH  ),(IN( 3),IVEC  )
C
      data DELTA /1.D-10/
C
      call HI ('EREBUS')
C     !BEG
      if((NO.gt.0).and.(IQPZT.gt.0)) then
C       (Get, and allocate, W allotment)
        call BESURE    (IN, IS, MOX, 'EREBUS')
C
        call PRIAM     (NO, 'ATMOSPHERE-2', 12)
        call LINER     (2, NO)
C
C----   Print
        call RUBEES    (NO, N, Z, TE, XNE, HND, ZT, DTE, R1N)
C
        call CONSTD    (TE, N, DELTA, ISO)
        if(.not.ISO) then
C----     Plot TE and DTE
          call ADARE   (NO, N, W(IVEC), TE, DTE, W(ILNE))
C----     Plot ZT
          call HALLETT (NO, N, Z, ZT)
        end if
C----   Plot XNE and HND
        call SABINE    (NO, N, W(IVEC), XNE, HND, W(ILNE), W(ILNH))
C
C       (Give back W allotment)
        call WGIVE     (W, 'EREBUS')
      end if
C     !END
      call BYE ('EREBUS')
C
      return
      end
