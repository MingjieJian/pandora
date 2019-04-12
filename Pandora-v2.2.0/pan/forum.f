      subroutine FORUM
     $(NAB,KWC,MRJ,LRJ,LZA,INPAIR,BANDL,BANDU,WAVES)
C
C     Rudolf Loeser, 1982 Feb 16
C---- Post-read input massage for part 1 of input.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, WAVES
      integer INPAIR, KB, KBX, KF, KK, KM, KOLEV, KOMPO, KR, KS, KWC,
     $        LRJ, LZA, MRJ, MS, NAB, NCP, NL, NOION, NRPMX, NS, NSHL,
     $        NT, NVX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(36),KS )
      equivalence (JZQ(37),KR )
      equivalence (JZQ(38),KB )
      equivalence (JZQ( 4),KF )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(42),NVX)
      equivalence (JZQ(10),KK )
      equivalence (JZQ(11),KBX)
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
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
      equivalence (KZQ( 33),KOLEV)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 4),NSHL )
      equivalence (LEST( 8),NRPMX)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(26),KOMPO)
C     !DASH
      external SLUNK, POUND, SAMPO, MARCEL, ADAM, INDARO, CYRUS, RUMOR,
     $         LAMMAS, THESEUS, EDEKI, HI, BYE
C
C               INPAIR(2,NT), MRJ(NSL+1), LRJ(NL), BANDL(NAB), LZA(50),
      dimension INPAIR(2,*),  MRJ(*),     LRJ(*),  BANDL(*),   LZA(*),
C
C               BANDU(NAB), WAVES(KWC)
     $          BANDU(*),   WAVES(*)
C
      call HI ('FORUM')
C     !BEG
      if(NOION.gt.0) then
        NL = 0
        NT = 0
      else
C----   Set up defaults for reference transition indices
        MS = INPAIR(1,1)
        NS = INPAIR(2,1)
      end if
C---- Inspect ZAUX-tables lengths and indices
      call SLUNK     (LZA)
C---- Restrain and massage population data counters
      call POUND
      call SAMPO
C---- Massage ion-data counters
      call MARCEL
C---- Massage level-array counters
      call LAMMAS    (MRJ, LRJ)
C---- Check other counter assumptions
      call ADAM
C---- Set up KK default
      call RUMOR     (MRJ, KOLEV, KK)
C---- Adjust NVX (if necessary)
      call EDEKI     (NVX)
C---- Compute NSHL and NRPMX
      call INDARO    (NSHL, NRPMX)
      if(NOION.le.0) then
C----   Set up line profile frequency counters
        call THESEUS (KS, KR, KB, KF, KBX, KM)
      end if
C---- Set up NCP, the number of composite line opacity wavelengths
      call CYRUS     (NAB, KOMPO, KWC, BANDL, BANDU, WAVES, NCP)
C     !END
      call BYE ('FORUM')
C
      return
      end
