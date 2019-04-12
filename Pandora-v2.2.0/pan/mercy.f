      subroutine MERCY
     $(X,W,N,PMG,H,Z,EF,XK,DD,V,IMG,EM)
C
C     Rudolf Loeser, 2006 Nov 28
C---- Computes EM (= M, the magnetic pressure term) for H.S.E.
C     (This is version 2 of MERCY.)
C     !DASH
      save
C     !DASH
      real*8 DD, DM, DP, EF, EM, H, PMG, V, W, X, XF, XK, Z
      integer I, IMG, N
      character LABEL*100
C     !DASH
      external DERIV1, DIVIDE, FELIPE, HI, BYE
C
      dimension X(*), W(*)
C
C               PMG(N), H(N), Z(N), EF(N), XK(N), DD(N), EM(N), IMG(N),
      dimension PMG(*), H(*), Z(*), EF(*), XK(*), DD(*), EM(*), IMG(*),
C
C               V(N)
     $          V(*)
C
      data LABEL /'M-integrand, for H.S.E.'/
C
      call HI ('MERCY')
C     !BEG
C---- Compute DD, derivative of magnetic pressure
      call DERIV1   (Z, PMG, DD, N)
C
C---- Compute XK (= K, for printing), and the integrand V
      do 100 I = 1,N
        XK(I) = exp(H(I))
        call DIVIDE (DD(I), XK(I), V(I))
  100 continue
C
C---- Integrate (call it EM)
      call FELIPE   (X, W, N, V, EM, LABEL, IMG)
C
C---- Compute final values of EM
      do 101 I = 1,N
        call DIVIDE (XK(I), EF(I), XF)
        EM(I) = XF*EM(I)
  101 continue
C     !END
      call BYE ('MERCY')
C
      return
      end
