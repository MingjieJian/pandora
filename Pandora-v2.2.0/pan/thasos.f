      subroutine THASOS
     $(N,XKL,ZAXA,ZAYA,SIGMAS,BHSNMS,F,XA,YA,OPAC,KHLF,DUMP)
C
C     Rudolf Loeser, 1982 Feb 01
C---- Computes XA and YA for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, BHSNUM, F, HALF, OPAC, SIGMA, SIGMAS, XA, XKL, YA,
     $       ZAXA, ZAYA, ZERO
      integer I, J, KHLF, N
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external DIVIDE, TREVES, NEARLY, HI, BYE
C
C               XKL(N,N), OPAC(N,N), ZAYA(N), XA(N,N), YA(N,N),
      dimension XKL(N,*), OPAC(N,*), ZAYA(*), XA(N,*), YA(N,*),
C
C               SIGMAS(N), BHSNMS(N), ZAXA(N)
     $          SIGMAS(*), BHSNMS(*), ZAXA(*)
C
      call HI ('THASOS')
C     !BEG
C---- Compute
      do 101 I = 1,N
        do 100 J = 1,N
          SIGMA  = F*SIGMAS(J)+XKL(I,J)*ZAXA(J)
          BHSNUM = F*BHSNMS(J)+XKL(I,J)*ZAYA(J)
C
          call DIVIDE (SIGMA,  OPAC(I,J), XA(I,J))
          call DIVIDE (BHSNUM, OPAC(I,J), YA(I,J))
  100   continue
  101 continue
C---- Test XA
      call NEARLY     (XA, (N*N), ZERO, HALF, KHLF)
C
      if(DUMP) then
        call TREVES   (N, XKL, ZAXA, ZAYA, BHSNMS, XA, YA)
      end if
C     !END
      call BYE ('THASOS')
C
      return
      end
