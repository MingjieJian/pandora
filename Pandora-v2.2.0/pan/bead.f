      subroutine BEAD
     $(N,XNH1,ELLH,XNHE11,ELLHE,XNHEP,ELLHE2,XNE,XNP,XNHE2K,ELLE,
     $ FION,FH,FHE,FHE2)
C
C     Rudolf Loeser, 1984 May 08
C---- Computes ion fractions for PENDANT.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, ELLE, ELLH, ELLHE, ELLHE2, FH, FHE, FHE2, FION,
     $       SUM, XNE, XNH1, XNHE11, XNHE2K, XNHEP, XNP, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, GROPE, HI, BYE
C
C               XNH1(N), ELLH(N), XNHE11(N), ELLHE(N), XNHEP(N), FH(N),
      dimension XNH1(*), ELLH(*), XNHE11(*), ELLHE(*), XNHEP(*), FH(*),
C
C               ELLHE2(N), XNE(N), XNP(N), XNHE2K(N), ELLE(N), FHE2(N),
     $          ELLHE2(*), XNE(*), XNP(*), XNHE2K(*), ELLE(*), FHE2(*),
C
C               FION(N), FHE(N)
     $          FION(*), FHE(*)
C
      call HI ('BEAD')
C     !BEG
      call ZERO1   (FION,N)
      call ZERO1   (FH  ,N)
      call ZERO1   (FHE ,N)
      call ZERO1   (FHE2,N)
C
      do 100 I = 1,N
        call GROPE (XNH1(I),ELLH(I),XNHE11(I),ELLHE(I),XNHEP(I),
     $              ELLHE2(I),XNE(I),XNP(I),XNHE2K(I),ELLE(I),A,B,C,D)
        SUM = A+B+C+D
        if(SUM.ne.ZERO) then
          FION(I) = D/SUM
          FH(I)   = A/SUM
          FHE(I)  = B/SUM
          FHE2(I) = C/SUM
        end if
  100 continue
C     !END
      call BYE ('BEAD')
C
      return
      end
