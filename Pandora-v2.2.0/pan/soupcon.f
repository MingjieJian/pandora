      subroutine SOUPCON
     $(N,X,F,FI,A,KE,NE,XE,FE,FIE,AE,DUMP,LABEL)
C
C     Rudolf Loeser, 1997 Dec 19
C---- Integrates: FI = integral of F(X); also returns the
C     component integrals in A.
C
C     Makes expanded versions of X and F to obtain expanded A and FI,
C     which are then collapsed back to regular size.
C     Expansion is done in logarithms.
C
C     See also POISON.
C     !DASH
      save
C     !DASH
      real*8 A, AE, F, FE, FI, FIE, SUM, X, XE
      integer KE, L, LU, LUEO, M, N, NE
      logical DUMP
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external VECOUT, PLAIN, SCRUNCH, BUTTEM, EXPAND, HELENA, COLAPSE,
     $         POM, HALT, DERE, PLAYN, MOVE1, HI, BYE
C
C               X(N), F(N), FI(N), A(N), XE(NE), FE(NE), FIE(NE), AE(NE)
      dimension X(*), F(*), FI(*), A(*), XE(*),  FE(*),  FIE(*),  AE(*)
C
      call HI ('SOUPCON')
C     !BEG
      LU = 0
      if(DUMP) then
        LU = LUEO
C----   Print header
        call POM (LU, KE, LABEL, X, F, N)
      end if
C     !EJECT
C---- Compute logs of integrand; abort if bad
      call PLAIN   (F, FI, N, LABEL)
      call VECOUT  (LU, FI, N, 'FL')
C
C---- Expand variable of integration
      call EXPAND  (KE, X, N, XE, NE)
      call VECOUT  (LU, XE, NE, 'XE')
C
C---- Expand integrand by quadratic interpolation
      call DERE    (X, 1, FI, 1, N, XE, 1, FE, 1, NE, 2)
      call VECOUT  (LU, FE, NE, 'FLE')
C
C---- Compute antilogs of expanded integrand; abort if bad
      call PLAYN   (FE, FE, NE, LABEL)
      call VECOUT  (LU, FE, NE, 'FE')
C
C---- Integrate parabolically
      call HELENA  (XE, 1, FE, 1, AE, 1, NE, SUM)
      call VECOUT  (LU, AE, NE, 'AE')
C
C---- Compute running integrals
      call MOVE1   (AE, NE, FIE)
      call BUTTEM  (FIE, 1, NE, SUM)
      call VECOUT  (LU, FIE, NE, 'IE')
C
C---- Collapse running integrals
      call COLAPSE (KE, FIE, NE, FI, L)
      call VECOUT  (LU, FI, L, 'I')
      if(L.ne.N) then
C----   Bail out - table lengths got mixed up
        write (MSSLIN(1),100) L,N
  100   format('L =',I12,', N =',I12,'; they should be equal.')
        call HALT  ('SOUPCON', 1)
      end if
C---- Collapse component integrals
      call SCRUNCH (KE, AE, NE, A, M)
      call VECOUT  (LU, A, M, 'A')
C
      if(M.ne.N) then
C----   Bail out - table lengths got mixed up
        write (MSSLIN(1),101) M,N
  101   format('M =',I12,', N =',I12,'; they should be equal.')
        call HALT  ('SOUPCON', 1)
      end if
C     !END
      call BYE ('SOUPCON')
C
      return
      end
