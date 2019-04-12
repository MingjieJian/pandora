      subroutine DUMBLY
     $(MN1,FB,GB,RB,SB,DELB,A,B,C,D,CHK)
C
C     Rudolf Loeser, 1997 Aug 25
C---- Prints original N1-recalculation intermediates.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DELB, FB, GB, RB, SB
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
C               FB(N), GB(N), RB(N), CHK(N), A(N), B(N), C(N), DELB(N),
      dimension FB(*), GB(*), RB(*), CHK(*), A(*), B(*), C(*), DELB(*),
C
C               D(N), SB(N)
     $          D(*), SB(*)
C
      call HI ('DUMBLY')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Original fourdiagonal equations, outward ',
     $           'solution.'///
     $       ' ',11X,'delta-bar',11X,'f-bar',11X,'g-bar',11X,'r-bar',
     $           11X,'s-bar')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,DELB(I),FB(I),GB(I),RB(I),SB(I),I=1,MN1)
  101 format(5(' ',I4,1P5E16.8/))
C
      call MUMBLY (MN1, A, B, C, D, CHK)
C     !END
      call BYE ('DUMBLY')
C
      return
      end
