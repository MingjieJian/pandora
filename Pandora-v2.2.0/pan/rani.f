      subroutine RANI
     $(N,R1N,Z,OPAC,SNU,XI,DX,FX,GX,TX,WVL,KTIT,DUMP)
C
C     Rudolf Loeser, 1978 Jun 30
C---- Computes Intensity, XI, for RAJA.
C     (This is version 2 of RANI.)
C     !DASH
      save
C     !DASH
      real*8 DX, FX, GX, OPAC, R1N, SNU, TX, WVL, XI, Z, ZERO, ZN
      integer I, IZT, KTIT, N
      logical DMPI, DUMP, KILROY
      character LABEL*127, qummy*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DAPHNIS, WINK, HI, BYE
C
C               OPAC(N), SNU(N), DX(NRPMX), FX(NRPMX), TX(NRPMX), Z(N),
      dimension OPAC(*), SNU(*), DX(*),     FX(*),     TX(*),     Z(*),
C
C               GX(NRPMX), XI(N)
     $          GX(*),     XI(*)
C
      call HI ('RANI')
C     !BEG
      KILROY = .true.
      IZT    = N/2
      ZN     = Z(N)
C
      XI(1)  = ZERO
C
      do 100 I = 2,N
        call DAPHNIS (DUMP, 2, I, IZT, WVL, KTIT, qummy, KILROY, LABEL,
     $                DMPI)
C
        call WINK    (I, Z, OPAC, SNU, DX, FX, GX, TX, ZN, R1N, DMPI,
     $                I, XI(I), LABEL)
  100 continue
C     !END
      call BYE ('RANI')
C
      return
      end
