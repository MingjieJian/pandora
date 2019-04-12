      subroutine POMPOUS
     $(N,X,F,G,A,LAB,DUMP,W)
C
C     Rudolf Loeser, 1997 Dec 19
C---- Computes G, the integral of F as a function of X.
C     Also returns the component integrals (as delivered by HELENA)
C     in A.
C
C     The actual quadratic integration uses and produces tables that
C     contain (KE+1) times as many points as the given tables;
C     the final result is collapsed back down to N points.
C     For this purpose, KE additional equispaced points are inserted
C     into successive intervals of the X table; the corresponding
C     values of F are obtained by quadratic interpolation in log(F).
C     Therefore, all values of F must be .gt. 0.
C
C     See also PAMPAS.
C     !DASH
      save
C     !DASH
      real*8 A, F, G, W, X
      integer IAE, IFE, IGE, IN, IS, IXE, KE, MOX, N, NE
      logical DUMP
      character LAB*(*)
C     !COM
C---- THETYS      as of 2000 Feb 11
      integer     MINTHE
      parameter   (MINTHE=3)
C     Number of "intermediate points" for the "standard"
C     second-order integration scheme.
C     .
C     !DASH
C     !EJECT
      external MIGNON, SOUPCON, MESHED, MASHED, WGIVE, HI, BYE
C
      dimension W(*)
C
C               X(N), F(N), G(N), A(N)
      dimension X(*), F(*), G(*), A(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IXE   ),(IN( 2),IGE   ),(IN( 3),IFE   ),(IN( 4),IAE   )
C
      call HI ('POMPOUS')
C     !BEG
      KE = MINTHE
      NE = (KE+1)*(N-1)+1
C
C     (Get, and allocate, W allotment)
      call MIGNON   (IN, IS, MOX, 'POMPOUS', NE)
C
      if(DUMP) then
        call MESHED ('POMPOUS', 2)
      end if
      call SOUPCON  (N, X, F, G, A, KE, NE, W(IXE), W(IGE), W(IFE),
     $               W(IAE), DUMP, LAB)
      if(DUMP) then
        call MASHED ('POMPOUS')
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'POMPOUS')
C     !END
      call BYE ('POMPOUS')
C
      return
      end
