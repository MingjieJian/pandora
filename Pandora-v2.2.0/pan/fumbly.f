      subroutine FUMBLY
     $(MN1,A,B,C,D,E,CHK)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Prints N1-recalculation intermediates, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, E
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
C               A(N), B(N), C(N), D(N), E(N), CHK(N)
      dimension A(*), B(*), C(*), D(*), E(*), CHK(*)
C
      call HI ('FUMBLY')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100)
  100 format(' ',19X,'A',15X,'B',15X,'C',15X,'D',15X,'E',11X,'check')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,A(I),B(I),C(I),D(I),E(I),CHK(I),I=1,MN1)
  101 format(5(' ',I4,1P6E16.8/))
C     !END
      call BYE ('FUMBLY')
C
      return
      end
