      subroutine GIN
     $(TI,TJ,Y,G)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Computes the interior columns of G(I,J) for QR Weight Matrix.
C     (TI is "TAU", TJ is "T".)
C     (This is version 2 of GIN.)
C     !DASH
      save
C     !DASH
      real*8 DT, E2T, E3D, E3T, E4D, E4T, EA3, EA4, G, HALF, HOMYOT,
     $       ONE, R, S, TI, TJ, TWTHRD, Y, YOT2, dummy
      integer KASE
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT(17),TWTHRD)
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  BOURBON, HALT, ASSUR, CLOVIS, EXPINT, DORIS, MARDUK,
     $          HI, BYE
      intrinsic abs
C     !EJECT
C
      call HI ('GIN')
C     !BEG
      call BOURBON    (TI, TJ, KASE)
      if((KASE.lt.1).or.(KASE.gt.5)) then
        write (MSSLIN(1),100) KASE
  100   format('KASE =',I12,', which is not 1, 2, 3, 4 or 5.')
        call HALT     ('GIN', 1)
      end if
C
      YOT2   = Y/(TJ**2)
      HOMYOT = HALF*(ONE-Y)/TJ
C
      if(KASE.eq.4) then
        call ASSUR    (TI, TJ, 3, EA3)
        call ASSUR    (TI, TJ, 4, EA4)
C
        G = YOT2*EA4+HOMYOT*EA3
C
      else
        DT = abs(TI-TJ)
        if((KASE.eq.1).or.(KASE.eq.3)) then
          call CLOVIS (TI, E2T, E3T, E4T)
          call EXPINT (3, DT, E3D, dummy)
          call EXPINT (4, DT, E4D, dummy)
        else
          call DORIS  (TI, E2T, E3T, E4T)
          call MARDUK (DT, 3, E3D)
          call MARDUK (DT, 4, E4D)
        end if
        if((KASE.eq.3).or.(KASE.eq.5)) then
          S = YOT2*(E4D-E4T)
        else if(KASE.eq.1) then
          S = YOT2*(TWTHRD-E4D-E4T)
        else
          R = TI/TJ
          S = (R-ONE)*(ONE-Y*R)-YOT2*(E4D+E4T)
        end if
C
        G   = S-HALF*(((ONE+Y)/TJ)*E3T+E2T)+HOMYOT*E3D
C
      end if
C     !END
      call BYE ('GIN')
C
      return
      end
