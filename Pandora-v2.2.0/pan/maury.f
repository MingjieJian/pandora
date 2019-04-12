      subroutine MAURY
     $(M,KBNDS,KINOUT,Z,F,G,R,S,A,B,C,D,E,Y,W,IW,DUMP)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Solves the five-diagonal form of the diffusion equations for y.
C     (This is version 2 of MAURY.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CRIT, D, E, F, G, R, S, W, Y, Z
      integer ICHK, IDEL, IN, IS, IW, KBNDS, KINOUT, M, MOX
      logical DUMP
C     !DASH
      external BEGGAR, JAGER, IMPALA, LEECH, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               J = N+MXTAP
C
C               Z(J), F(J), G(J), R(J), S(J), A(J), B(J), C(J), D(J),
      dimension Z(*), F(*), G(*), R(*), S(*), A(*), B(*), C(*), D(*),
C
C               E(J), Y(J)
     $          E(*), Y(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IDEL  ),(IN( 2),ICHK  )
C
      data CRIT /1.D-7/
C
      call HI ('MAURY')
C     !BEG
C     (Get, and allocate, W allotment)
      call JAGER   (IN, IS, MOX, 'MAURY', M)
C
C---- Compute DEL
      call BEGGAR  (M, Z, W(IDEL), CRIT, 'MAURY')
C---- Set up and solve system of equations
      call IMPALA  (M, KBNDS, KINOUT, W(IDEL), F, G, R, S, A, B, C, D,
     $              E, Y, W(ICHK), W, IW)
      if(DUMP) then
        call LEECH (M, F, G, R, S, W(IDEL), A, B, C, D, E, W(ICHK))
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'MAURY')
C     !END
      call BYE ('MAURY')
C
      return
      end
