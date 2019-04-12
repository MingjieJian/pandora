      subroutine CRUST
     $(N,SAB,U,HNP,ITAU)
C
C     Rudolf Loeser, 1987 Sep 11
C---- Prints error data and aborts, for PEANUT.
C     !DASH
      save
C     !DASH
      real*8 HNP, SAB, U
      integer I, ITAU, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, ABORT, HI, BYE
C
C               SAB(N), U(N), HNP(N)
      dimension SAB(*), U(*), HNP(*)
C
      call HI ('CRUST')
C     !BEG
      call MESHED ('CRUST', 1)
      write (LUEO,100) ITAU
  100 format(' ','Trouble at depth ',I3,'; calculating NE from ',
     $           'quadratic expression.')
      call LINER  (2, LUEO)
      write (LUEO,101)
  101 format(' ',18X,'SAB',15X,'U',13X,'HNP')
      call LINER  (1, LUEO)
      write (LUEO,102) (I,SAB(I),U(I),HNP(I),I=1,N)
  102 format(5(' ',I5,1P3E16.8/))
      call ABORT
C     !END
      call BYE ('CRUST')
C
      return
      end
