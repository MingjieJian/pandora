      subroutine FURZE
     $(DX,F,G,N,FI,T,IMAX,DUMP,IA,KODE)
C
C     Rudolf Loeser, 1981 Aug 30
C---- Computes the intensity FI, emerging along a particular ray,
C     traversing N shells in spherical coordinates.
C     Also returns T, the optical depth along that ray.
C     (NOTE: F is Opacity, G is Monochromatic Source Function.)
C     (This is version 3 of FURZE.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, DX, EFDX, F, FDX, FI, G, ONE, PEFDX, T, TERM, XK,
     $       ZERO
      integer I, IA, IMAX, J, KODE, N
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, ULEX, WHIN, QEXP1, HI, BYE
C
C               DX(N), F(N), G(N), T(N)
      dimension DX(*), F(*), G(*), T(*)
C
      data CRIT /1.D-5/
C     !EJECT
C
      call HI ('FURZE')
C     !BEG
      if(DUMP) then
        call ULEX   (KODE, IA)
      end if
C
      call ZERO1    (T, N)
C
      J  = 1
      FI = ZERO
      PEFDX = ONE
      do 100 I = 1,N
        IMAX = I
        FDX  = F(I)*DX(I)
        T(I) = T(J)+FDX
        J    = I
        call QEXP1  (FDX, EFDX, 0, XK)
        TERM = PEFDX*G(I)*XK
        FI   = FI+TERM
        if(DUMP) then
          call WHIN (I, FDX, EFDX, PEFDX, XK, TERM, FI)
        end if
C
        if((TERM.lt.CRIT*FI).and.(PEFDX.lt.CRIT)) then
          goto 101
        end if
C
        PEFDX = EFDX*PEFDX
  100 continue
C
  101 continue
C     !END
      call BYE ('FURZE')
C
      return
      end
