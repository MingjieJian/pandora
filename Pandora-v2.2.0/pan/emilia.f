      subroutine EMILIA
     $(NO,N,HND,RHAB,H1,HK)
C
C     Rudolf Loeser, 2001 Dec 18
C---- Prints Hydrogen data, for MEULAN.
C     (This is version 2 of EMILIA.)
C     !DASH
      save
C     !DASH
      real*8 H1, HK, HND, RHAB
      integer I, N, NO
C     !DASH
      external LINER, HI, BYE
C
C               HND(N), RHAB(N), H1(N), HK(N)
      dimension HND(*), RHAB(*), H1(*), HK(*)
C
      call HI ('EMILIA')
C     !BEG
      write (NO,100)
  100 format(' ',10X,'total Hydrogen',10X,'abd. ratio',
     $           10X,'H(level 1)',3X,'protons = H(cont)'/
     $       ' ',21X,'HND',16X,'RHAB',18X,'H1',18X,'HK')
      call LINER (1,NO)
      write (NO,101) (I,HND(I),RHAB(I),H1(I),HK(I),I=1,N)
  101 format(5(' ',I4,1P4E20.13/))
C     !END
      call BYE ('EMILIA')
C
      return
      end
