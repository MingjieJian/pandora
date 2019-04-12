      subroutine LAMMAS
     $(MRJ,LRJ)
C
C     Rudolf Loeser, 2002 Jul 12
C---- Massages level-array counters.
C     !DASH
      save
C     !DASH
      integer IMAX, LRJ, MLR, MLS, MMR, MRJ, MRS, NL, NP, NSL, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(18),MMR)
      equivalence (JZQ(28),MLR)
      equivalence (JZQ(29),MRS)
      equivalence (JZQ(33),MLS)
C     !DASH
      external IRRSUM, MINMAXI, HI, BYE
C
C               MRJ(NSL+1), LRJ(NL)
      dimension MRJ(*),     LRJ(*)
C
      call HI ('LAMMAS')
C     !BEG
      NP = NSL+1
      call IRRSUM  (MRJ,NP,MRS)
      call MINMAXI (MRJ,1,NP,jummy,IMAX)
      MMR = MRJ(IMAX)
C
      call IRRSUM  (LRJ,NL,MLS)
      call MINMAXI (LRJ,1,NL,jummy,IMAX)
      MLR = LRJ(IMAX)
C     !END
      call BYE ('LAMMAS')
C
      return
      end
