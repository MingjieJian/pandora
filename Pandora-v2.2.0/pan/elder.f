      subroutine ELDER
     $(I,EV,S,ELL,Z,A,XNE,XNP,XNH1,XNHE11,XNHEP,XNHE2K,
     $ ELLH,ELLE,ELLHE2,ELLHE,DUMP)
C
C     Rudolf Loeser, 1984 May 08
C---- Computes EV, and the corresponding S, for LOCKET.
C     !DASH
      save
C     !DASH
      real*8 A, ARG, DZ, ELL, ELLE, ELLH, ELLHE, ELLHE2, EV, S, XNE,
     $       XNH1, XNHE11, XNHE2K, XNHEP, XNP, Z
      integer I
      logical DUMP
C     !DASH
      external  ESS, HI, BYE
      intrinsic abs
C
C               EV(N), S(N), XNP(N), XNH1(N), ELLE(N), ELLH(N), XNE(N),
      dimension EV(*), S(*), XNP(*), XNH1(*), ELLE(*), ELLH(*), XNE(*),
C
C               XNHE11(N), XNHEP(N), XNHE2K(N), ELLHE2(N), A(N), Z(N),
     $          XNHE11(*), XNHEP(*), XNHE2K(*), ELLHE2(*), A(*), Z(*),
C
C               ELLHE(N)
     $          ELLHE(*)
C
      call HI ('ELDER')
C     !BEG
      DZ = abs(Z(I-1)-Z(I))
C
C---- Compute 'predicted' EV(I),
      ARG   = (EV(I-1)**2)+S(I-1)*DZ
      EV(I) = sqrt(ARG)
C---- and corresponding S(I) (and LE, LH, LHE, LHE2).
      call ESS (I, EV, S, ELL, A, ELLE, ELLH, ELLHE, ELLHE2, XNE,
     $          XNP, XNH1, XNHE11, XNHEP, XNHE2K, DUMP)
C
C---- Now compute final EV(I),
      ARG   = (EV(I-1)**2)+S(I)*DZ
      EV(I) = sqrt(ARG)
C---- and the corresponding final S(I) (etc.).
      call ESS (I, EV, S, ELL, A, ELLE, ELLH, ELLHE, ELLHE2, XNE,
     $          XNP, XNH1, XNHE11, XNHEP, XNHE2K, DUMP)
C
C     !END
      call BYE ('ELDER')
C
      return
      end
