      subroutine PAMPAS
     $(N,X,F,G,LABEL,DUMP,W)
C
C     Rudolf Loeser, 1980 Sep 23
C---- Computes G, the integral of F as a function of X.
C
C     The actual quadratic integration uses and produces tables that
C     contain (KE+1) times as many points as the given tables;
C     the final result is collapsed back down to N points.
C     For this purpose, KE additional equispaced points are inserted
C     into successive intervals of the X table; the corresponding
C     values of F are obtained by quadratic interpolation in log(F).
C     Therefore, all values of F must be .gt. 0.
C     (This is version 2 of PAMPAS.)
C
C     See also POMPOUS.
C     !DASH
      save
C     !DASH
      real*8 F, G, W, X
      integer IFE, IGE, IN, IS, IXE, KE, MOX, N, NE
      logical DUMP
      character LABEL*(*)
C     !COM
C---- THETYS      as of 2000 Feb 11
      integer     MINTHE
      parameter   (MINTHE=3)
C     Number of "intermediate points" for the "standard"
C     second-order integration scheme.
C     .
C     !DASH
C     !EJECT
      external MERCURY, POISON, MESHED, MASHED, WGIVE, HI, BYE
C
      dimension W(*)
C
C               X(N), F(N), G(N)
      dimension X(*), F(*), G(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXE   ),(IN( 2),IGE   ),(IN( 3),IFE   )
C
      call HI ('PAMPAS')
C     !BEG
      KE = MINTHE
      NE = (KE+1)*(N-1)+1
C
C     (Get, and allocate, W allotment)
      call MERCURY  (IN, IS, MOX, 'PAMPAS', NE)
C
      if(DUMP) then
        call MESHED ('PAMPAS', 2)
      end if
      call POISON   (N, X, F, G, KE, NE, W(IXE), W(IGE), W(IFE),
     $               DUMP, LABEL)
      if(DUMP) then
        call MASHED ('PAMPAS')
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'PAMPAS')
C     !END
      call BYE ('PAMPAS')
C
      return
      end
