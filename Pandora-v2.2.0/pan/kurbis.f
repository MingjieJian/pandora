      subroutine KURBIS
     $(IVLSW,KDIAG,KBNDS,KINOUT,I4DIO,I4DFM,I4DEQ,ITER,DUMP,W,IW,IMG,
     $ M,Z,F,G,R,S,H,ZXH,Y,CHK,KZANX)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Computes Y and CHK, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 CHK, F, G, H, R, S, W, Y, Z, ZXH
      integer I4DEQ, I4DFM, I4DIO, IA, IB, IC, ID, IE, IFO, IMG, IN, IS,
     $        ITER, IVEC, IVLSW, IW, KBNDS, KDIAG, KINOUT, KZANX, M,
     $        MOX
      logical DUMP
C     !DASH
      external WALLA, MAURY, ROBATS, SWASH, EDY, KURNAI, MELON, WGIVE,
     $         HI, BYE
C
C               J = N+MXTAP
C
      dimension W(*), IW(*)
C
C               IMG(J), ZXH(J), Z(J), F(J), G(J), R(J), S(J), KZANX(J),
      dimension IMG(*), ZXH(*), Z(*), F(*), G(*), R(*), S(*), KZANX(*),
C
C               CHK(J), H(J), Y(J)
     $          CHK(*), H(*), Y(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IA    ),(IN( 2),IB    ),(IN( 3),IC    ),(IN( 4),ID    ),
     $(IN( 5),IE    ),(IN( 6),IFO   ),(IN( 7),IVEC  )
C     !EJECT
C
      call HI ('KURBIS')
C     !BEG
C     (Get, and allocate, W allotment)
      call MELON      (IN, IS, MOX, 'KURBIS', M)
C
C---- Set up and solve the systems of equations for Y
      if(IVLSW.eq.0) then
        if(KDIAG.eq.3) then
C----     Tridiagonal equations
          call WALLA  (M, KBNDS, KINOUT, Z, F, G, R, S, W(IA), W(IB),
     $                 W(IC), Y, W(IVEC), W, DUMP)
        else if(KDIAG.eq.5) then
C----     Five-diagonal equations
          call MAURY  (M, KBNDS, KINOUT, Z, F, G, R, S, W(IA), W(IB),
     $                 W(IC), W(ID), W(IE), Y, W, IW, DUMP)
        else if(KDIAG.eq.4) then
C----     Four-diagonal equations
          call KURNAI (M, KINOUT, I4DIO, I4DFM, I4DEQ, Z, F, G, R, S,
     $                 W(IA), W(IB), W(IC), W(ID), Y, W(IVEC), W, IW,
     $                 DUMP)
        end if
C
C----   Edit possible negatives
        call EDY      (Y, M, IMG, W(IFO), ITER)
C
      else
C----   Use "new method for treating mass-flows"
        call SWASH    (M, IVLSW, Z, R, S, H, ZXH, Y, W, DUMP)
      end if
C
C---- Verify the results, and get CHK
      call ROBATS     (M, Z, F, G, R, S, Y, CHK, KZANX, W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE      (W, 'KURBIS')
C     !END
      call BYE ('KURBIS')
C
      return
      end
