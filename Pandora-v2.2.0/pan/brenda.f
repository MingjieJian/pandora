      subroutine BRENDA
     $(X,IX,W,IW,XPBL,RKI,IQRK,RLI,IQRL)
C
C     Rudolf Loeser, 1991 Feb 15
C---- Computes additional terms affecting RK and RL, for ROPE.
C     (This is version 3 of BRENDA.)
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, W, X, XPBL
      integer IQRK, IQRL, IW, IX, KOLEV, MO, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external BARB, GOAT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RKI(N,NSL), RLI(N,NSL), XPBL(Lenpbl), IQRK(NSL),
      dimension RKI(*),     RLI(*),     XPBL(*),      IQRK(*),
C
C               IQRL(NSL)
     $          IQRL(*)
C
      call HI ('BRENDA')
C     !BEG
      call BARB (IQRK, IQRL, NSL, KOLEV)
      call GOAT (X, IX, W, IW, XPBL, RKI, IQRK, RLI, IQRL, MO)
C     !END
      call BYE ('BRENDA')
C
      return
      end
