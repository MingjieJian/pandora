      subroutine SEMEX
     $(NO,METSE)
C
C     Rudolf Loeser, 2004 Apr 13
C---- Prints explanation of statistical equilibrium solution.
C     !DASH
      save
C     !DASH
      integer J, METSE, NO
      character TIT*10
C     !DASH
      external  LINER, DASHER, HI, BYE
      intrinsic min, max
C
      dimension TIT(5)
C
      data TIT /'NOVA.     ', 'COMPLEX-U.', 'COMPLEX-L.',
     $          'CHAIN.    ', 'VAMOS.    '/
C
      call HI ('SEMEX')
C     !BEG
      call LINER  (1, NO)
C
      J = max(min(METSE,4),0)+1
      write (NO,100) TIT(J),METSE
  100 format(' ','----- Values of EP and BS use the statistical ',
     $           'equilibrium terms PE and FE computed by ',A10,
     $           ' (METSE =',I2,')')
C
      call DASHER (NO)
C     !END
      call BYE ('SEMEX')
C
      return
      end
