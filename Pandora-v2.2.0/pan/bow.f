      subroutine BOW
     $(N,X,NDT,A,SIG,XL,XH,YL,YH)
C
C     Rudolf Loeser, 2002 Jan 08
C---- Sets up plot limits, for HORN.
C     (This is version 2 of BOW.)
C     !DASH
      save
C     !DASH
      real*8 A, ONE, SIG, X, XH, XL, YH, YL, ZERO
      integer IMAX, IMIN, N, NDT, NN
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
      external LOGO, MNMXD, BELOWD, ABOVED, HI, BYE
C
C               X(N), A(N,NDT)
      dimension X(*), A(*)
C
      call HI ('BOW')
C     !BEG
      NN = N*NDT
      call LOGO   (A, NN, 1, SIG, A)
      call MNMXD  (A, 1, NN, SIG, IMIN, IMAX)
C
      call BELOWD (A(IMIN), ONE, YL)
      call ABOVED (A(IMAX), ONE, YH)
C
      XL = ZERO
      XH = X(NDT)
C     !END
      call BYE ('BOW')
C
      return
      end
