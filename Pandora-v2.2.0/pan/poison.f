      subroutine POISON
     $(N,X,F,FI,KE,NE,XE,FE,FIE,DUMP,LABEL)
C
C     Rudolf Loeser, 1979 Oct 30
C---- Integrates: FI = integral of F(X).
C
C     Makes expanded versions of X and F to obtain an expanded FI,
C     which is then collapsed back to regular size.
C     Expansion is done in logarithms.
C     (Completely new, revised version.)
C
C     See also SOUPCON.
C     !DASH
      save
C     !DASH
      real*8 F, FE, FI, FIE, X, XE
      integer KE, L, LU, LUEO, N, NE
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
      external VECOUT, PLAIN, EXPAND, DERE, PLAYN, BUSH, COLAPSE, HALT,
     $         POM, HI, BYE
C
C               X(N), F(N), FI(N), XE(NE), FE(NE), FIE(NE)
      dimension X(*), F(*), FI(*), XE(*),  FE(*),  FIE(*)
C
      call HI ('POISON')
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
      call BUSH    (XE, 1, FE, 1, FIE, 1, NE)
      call VECOUT  (LU, FIE, NE, 'IE')
C
C---- Collapse integral
      call COLAPSE (KE, FIE, NE, FI, L)
      call VECOUT  (LU, FI, L, 'I')
C
      if(L.ne.N) then
C----   Bail out - table lengths got mixed up
        write (MSSLIN(1),100) L,N
  100   format('L =',I12,', N =',I12,'; they should be equal.')
        call HALT  ('POISON', 1)
      end if
C     !END
      call BYE ('POISON')
C
      return
      end
