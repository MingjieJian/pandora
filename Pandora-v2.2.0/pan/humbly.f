      subroutine HUMBLY
     $(MN1,F,G,R,S,DEL,A,B,C,D,CHK)
C
C     Rudolf Loeser, 1997 Aug 25
C---- Prints original N1-recalculation intermediates.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DEL, F, G, R, S
      integer I, LUEO, MN1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUMBLY, LINER, HI, BYE
C
C               F(N), G(N), R(N), S(N), DEL(N), A(N), B(N), C(N), D(N),
      dimension F(*), G(*), R(*), S(*), DEL(*), A(*), B(*), C(*), D(*),
C
C               CHK(N)
     $          CHK(*)
C
      call HI ('HUMBLY')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Original fourdiagonal equations, inward ',
     $           'solution.'///
     $       ' ',15X,'delta',15X,'f',15X,'g',15X,'r',15X,'s')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,DEL(I),F(I),G(I),R(I),S(I),I=1,MN1)
  101 format(5(' ',I4,1P5E16.8/))
C
      call MUMBLY (MN1, A, B, C, D, CHK)
C     !END
      call BYE ('HUMBLY')
C
      return
      end
