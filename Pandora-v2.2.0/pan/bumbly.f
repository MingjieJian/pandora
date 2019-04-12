      subroutine BUMBLY
     $(MN1,A,B,C,CHK)
C
C     Rudolf Loeser, 1997 Jul 29
C---- Prints N1-recalculation intermediates, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK
      integer I, LUEO, MN1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               A(N), B(N), C(N), CHK(N)
      dimension A(*), B(*), C(*), CHK(*)
C
      call HI ('BUMBLY')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100)
  100 format(' ','Tridiagonal equations'//
     $       ' ',18X,'-A',15X,'B',14X,'-C',11X,'check')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,A(I),B(I),C(I),CHK(I),I=1,MN1)
  101 format(5(' ',I4,1P4E16.8/))
C     !END
      call BYE ('BUMBLY')
C
      return
      end
