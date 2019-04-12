      subroutine VERBENA
     $(XPBL,HN1,HNK,HE1N1,HE2N1,HE2NK)
C
C     Rudolf Loeser, 1981 Feb 03
C---- Gets Populations Input Data, for DAFFY.
C     !DASH
      save
C     !DASH
      real*8 HE1N1, HE2N1, HE2NK, HN1, HNK, XPBL, dummy
C     !DASH
      external POPUTIL, MOVE1, HI, BYE
C
C               HE2N1(N), HE2NK(N), HE1N1(N), HN1(N), XPBL(Lenpbl),
      dimension HE2N1(*), HE2NK(*), HE1N1(*), HN1(*), XPBL(*),
C
C               HNK(N)
     $          HNK(*)
C
      call HI ('VERBENA')
C     !BEG
C---- Helium II
      call POPUTIL (XPBL,5, 1,HE2N1,1,HE2NK,0,dummy,0,dummy)
C---- Helium I
      call POPUTIL (XPBL,4, 1,HE1N1,0,dummy,0,dummy,0,dummy)
C---- Hydrogen
      call POPUTIL (XPBL,1, 1,HN1  ,1,HNK  ,0,dummy,0,dummy)
C     !END
      call BYE ('VERBENA')
C
      return
      end
