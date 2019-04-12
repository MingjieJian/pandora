      subroutine CALLUNA
     $(EP1,EP2,BDK,EP1MN,N)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Edits, for HEATHER.
C     !DASH
      save
C     !DASH
      real*8 BDK, E, EP1, EP1MN, EP2, R, TWO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               EP1(N), EP2(N), BDK(N)
      dimension EP1(*), EP2(*), BDK(*)
C
      call HI ('CALLUNA')
C     !BEG
      E = -TWO*EP1MN
      do 100 I = 1,N
        call DIVIDE (E,BDK(I),R)
        EP1(I) = EP1(I)+E
        EP2(I) = EP2(I)+R
  100 continue
C     !END
      call BYE ('CALLUNA')
C
      return
      end
