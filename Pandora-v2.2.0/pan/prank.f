      subroutine PRANK
     $(LEV,N,TE,ALL)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Computes a lower-level charge exchange term for O-I.
C     (This is version 2 of PRANK.)
C     !DASH
      save
C     !DASH
      real*8 ALL, ARG, CSP, DNUK, EXT, F, P, TE
      integer I, L, LEV, N
C     !DASH
      external  PROD, HI, BYE
      intrinsic min, max
C
C               TE(N), ALL(N)
      dimension TE(*), ALL(*)
C
      dimension P(3), DNUK(3)
C
      data CSP  /9.D0/
      data P    /5.D0, 3.D0, 1.D0/
      data DNUK /0.D0, 4.745D-3, 6.805D-3/
C
      call HI ('PRANK')
C     !BEG
      L = min(max(LEV,1),3)
      F = P(L)/CSP
C
      do 100 I = 1,N
        call PROD (TE(I), DNUK(L), 1, ARG, EXT)
        ALL(I) = F*EXT
  100 continue
C     !END
      call BYE ('PRANK')
C
      return
      end
