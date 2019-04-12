      subroutine LING
     $(ZETA,DZETA,N)
C
C     Rudolf Loeser, 1997 Oct 22
C---- Prints zeta and delta, to more figures, for N1-recalculation.
C     (This is version 2 of LING.)
C     !DASH
      save
C     !DASH
      real*8 DZETA, ZETA
      integer LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DVECOUT, HI, BYE
C
C               ZETA(N), DZETA(N)
      dimension ZETA(*), DZETA(*)
C
      call HI ('LING')
C     !BEG
      call LINER   (2, LUEO)
      write (LUEO,100)
  100 format(' ','DZETA = zeta differences, i.e. component integrals')
      call DVECOUT (LUEO, ZETA,  N, 'ZETA' )
      call DVECOUT (LUEO, DZETA, N, 'DZETA')
C     !END
      call BYE ('LING')
C
      return
      end
