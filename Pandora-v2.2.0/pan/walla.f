      subroutine WALLA
     $(M,KBNDS,KINOUT,Z,F,G,R,S,A,B,C,Y,VEC,W,DUMP)
C
C     Rudolf Loeser, 1988 Jun 21
C---- Solves the tridiagonal form of the diffusion equation for y.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, F, G, R, S, VEC, W, Y, Z
      integer IFF, IGG, IN, IS, IZZ, KBINN, KBNDS, KBOUT, KINOUT, M,
     $        MOX
      logical DUMP
C     !DASH
      external NALLA, MONGREL, CALLA, TRIDAG, BUMBLY, MALL, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               J = N+MXTAP
C
C               Y(J), Z(J), F(J), G(J), R(J), S(J), A(J), VEC(J), C(J),
      dimension Y(*), Z(*), F(*), G(*), R(*), S(*), A(*), VEC(*), C(*),
C
C               B(J)
     $          B(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IZZ   ),(IN( 2),IFF  ),(IN( 3),IGG   )
C
      call HI ('WALLA')
C     !BEG
C     (Get, and allocate, W allotment)
      call NALLA    (IN, IS, MOX, 'WALLA', M)
C
      call MONGREL  (KBNDS, KINOUT, KBINN, KBOUT)
      call CALLA    (M, Z,      F,      G,      R, KBINN, KBOUT, A, B,
     $               C, W(IZZ), W(IFF), W(IGG))
      call TRIDAG   (A, B, C, S, Y, M, VEC)
      call MALL     (A, B, C, Y, S, VEC, M)
      if(DUMP) then
        call BUMBLY (M, A, B, C, VEC)
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'WALLA')
C     !END
      call BYE ('WALLA')
C
      return
      end
