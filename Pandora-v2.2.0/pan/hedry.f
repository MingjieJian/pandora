      subroutine HEDRY
     $(BANDL,BANDU,BANDY,WAVCO,YWVCO,INWVC,WAVES)
C
C     Rudolf Loeser, 1983 Jun 30
C---- Sets up data for wavelengths at which Composite Line Opacity
C     is specified.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, BANDY, WAVCO, WAVES, YWVCO
      integer INWVC, KOMNP, KOMNT, KOMNV, KOMPO, KWC, NAB, NCP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(46),KWC)
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
      equivalence (KZQ( 69),KOMNP)
      equivalence (KZQ( 70),KOMNT)
      equivalence (KZQ( 68),KOMNV)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(26),KOMPO)
C     !DASH
      external GUPTA, HI, BYE
C
C               BANDL(NAB), BANDU(NAB), BANDY(NAB), WAVCO(NCP),
      dimension BANDL(*),   BANDU(*),   BANDY(*),   WAVCO(*),
C
C               YWVCO(NCP), INWVC(NCP), WAVES(KWC)
     $          YWVCO(*),   INWVC(*),   WAVES(*)
C     !EJECT
C
      call HI ('HEDRY')
C     !BEG
      if(NCP.gt.0) then
        call GUPTA  (KOMPO,KOMNP,KOMNT,KOMNV,BANDL,BANDU,BANDY,NAB,
     $               WAVCO,YWVCO,INWVC,NCP,WAVES,KWC)
      end if
C     !END
      call BYE ('HEDRY')
C
      return
      end
