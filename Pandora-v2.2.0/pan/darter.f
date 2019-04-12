      subroutine DARTER
     $(NO,NR,K,DL,Z,ARRAY)
C
C     Rudolf Loeser, 1985 Jun 20
C---- Prints arrays, for BANANA.
C     !DASH
      save
C     !DASH
      real*8 ARRAY, DL, Z
      integer I, J, K, NO, NR
C     !DASH
      external LINER, SHIM, HI, BYE
C
C               DL(KM), Z(N), ARRAY(N ,KM)
      dimension DL(*),  Z(*), ARRAY(NR,*)
C
C
      call HI ('DARTER')
C     !BEG
      call LINER  (1,NO)
      write (NO,100) (Z(I),I=1,NR)
  100 format(' ',5X,'DL',6X,'Z',1P10E11.3)
      call LINER  (1,NO)
C
      do 102 J = 1,K
        write (NO,101) DL(J),(ARRAY(I,J),I=1,NR)
  101   format(' ',1PE10.3,4X,10E11.4)
        call SHIM (J,5,NO)
  102 continue
C     !END
      call BYE ('DARTER')
C
      return
      end
