      subroutine PENDANT
     $(N,Z,TE,XNE,XNHK,XNH1,XNHE1K,XNHE11,XNHE2K,XNHE21,NS,ELL,EMAX,
     $ LU,S,EV,W)
C
C     Rudolf Loeser, 1984 May 08
C---- Calculates particle energy EV, and particle energy dissipation S.
C     LU is the unit for regular printout from the fast-electrons
C     calculation.
C     !DASH
      save
C     !DASH
      real*8 ELL, EMAX, EV, S, TE, W, XNE, XNH1, XNHE11, XNHE1K, XNHE21,
     $       XNHE2K, XNHK, Z
      integer IA, IE, IELHE2, IELLE, IELLH, IELLHE, IFH, IFHE, IFHE2,
     $        IFION, IN, IS, IXNHEP, LU, MOX, N, NS
C     !DASH
      external BAUBLE, CATCH, LOCKET, BEAD, PASTE, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), TE(N), XNE(N), XNHK(N), XNHE21(N), S(N), XNH1(N),
      dimension Z(*), TE(*), XNE(*), XNHK(*), XNHE21(*), S(*), XNH1(*),
C
C               XNHE1K(N), XNHE11(N), XNHE2K(N), EV(N)
     $          XNHE1K(*), XNHE11(*), XNHE2K(*), EV(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IXNHEP),(IN( 2),IA    ),(IN( 3),IELLH ),(IN( 4),IELLE ),
     $(IN( 5),IELHE2),(IN( 6),IELLHE),(IN( 7),IFION ),(IN( 8),IFH   ),
     $(IN( 9),IFHE  ),(IN(10),IFHE2 ),(IN(11),IE    )
C     !EJECT
C
      call HI ('PENDANT')
C     !BEG
C     (Get, and allocate, W allotment)
      call BAUBLE  (IN, IS, MOX, 'PENDANT')
C
C---- Get auxiliary tables NHEP and A
      call CATCH   (N, TE, XNE, XNHE1K, XNHE21, W(IA), W(IXNHEP))
C---- Compute S and EV, and auxiliaries
      call LOCKET  (N, Z, TE, W(IA), XNE, XNHK, XNH1, XNHE11,
     $              W(IXNHEP), XNHE2K, NS, ELL, EMAX, LU, S, W(IE),
     $              EV, W(IELLH), W(IELLE), W(IELHE2), W(IELLHE))
C
      if(LU.gt.0) then
C----   Compute ion fractions (for printing only)
        call BEAD  (N, XNH1, W(IELLH), XNHE11, W(IELLHE), W(IXNHEP),
     $              W(IELHE2), XNE, XNHK, XNHE2K, W(IELLE), W(IFION),
     $              W(IFH), W(IFHE), W(IFHE2))
C----   Print
        call PASTE (LU, N, NS, ELL, EMAX, Z, S, W(IE), EV, W(IFION),
     $              W(IFH), W(IFHE), W(IFHE2))
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'PENDANT')
C     !END
      call BYE ('PENDANT')
C
      return
      end
