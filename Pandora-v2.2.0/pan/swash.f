      subroutine SWASH
     $(M,IVLSW,Z,R,S,H,ZXH,Y,W,DUMP)
C
C     Rudolf Loeser, 1997 Aug 05
C---- Uses the exponential method ("new method") to compute y, for
C     the "Special N1" calculation of the diffusion calculation.
C     (This is version 2 of SWASH.)
C     !DASH
      save
C     !DASH
      real*8 H, R, S, W, Y, Z, ZXH
      integer IA, IHB, IN, IP, IRB, IS, ISB, ISIGMA, IVLSW, IZB, IZETA,
     $        M, MOX
      logical DUMP
C     !DASH
      external ARRDIV, MOVE1, REVERSD, WILLI, NEGATE, TUMBLY, CRUMBLY,
     $         LOSS, WGIVE, HI, BYE
C
      dimension W(*)
C
C               J = N+MXTAP
C
C               Z(J), R(J), S(J), H(J), Y(J), ZXH(J)
      dimension Z(*), R(*), S(*), H(*), Y(*), ZXH(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IP    ),(IN( 2),ISIGMA),(IN( 3),IZETA ),(IN( 4),ISB   ),
     $(IN( 5),IA    ),(IN( 6),IHB   ),(IN( 7),IZB   ),(IN( 8),IRB   )
C
      call HI ('SWASH')
C     !BEG
C     (Get, and allocate, W allotment)
      call LOSS        (IN, IS, MOX, 'SWASH', M)
C
      if(IVLSW.ge.0) then
        call WILLI     (M, Z, H, R, S, W(IP), W(ISIGMA), W(IZETA),
     $                  Y, W(IA), DUMP, W)
        if(DUMP) then
          call CRUMBLY (M, IVLSW, Z, ZXH, W(IP), W(ISIGMA), H, R, S,
     $                  W(IZETA), Y, W(IA))
        end if
        call ARRDIV    (Y, H, Y, M)
C     !EJECT
      else
        call MOVE1    (H, M, W(IHB))
        call REVERSD  (W(IHB), 1, M)
        call NEGATE   (W(IHB), M)
C
        call MOVE1    (Z, M, W(IZB))
        call REVERSD  (W(IZB), 1, M)
        call NEGATE   (W(IZB), M)
C
        call MOVE1    (R, M, W(IRB))
        call REVERSD  (W(IRB), 1, M)
C
        call MOVE1    (S, M, W(ISB))
        call REVERSD  (W(ISB), 1, M)
C
        call WILLI    (M, W(IZB), W(IHB), W(IRB), W(ISB), W(IP),
     $                 W(ISIGMA), W(IZETA), Y, W(IA), DUMP, W)
        if(DUMP) then
          call TUMBLY (M, IVLSW, W(IZB), ZXH, W(IP), W(ISIGMA),
     $                 W(IHB), W(IRB), W(ISB), W(IZETA), Y, W(IA))
        end if
        call ARRDIV   (Y, W(IHB), Y, M)
        call REVERSD  (Y, 1, M)
      end if
C
C     (Give back W allotment)
      call WGIVE      (W, 'SWASH')
C     !END
      call BYE ('SWASH')
C
      return
      end
