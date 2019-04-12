      subroutine STROPHE
     $(IN,MUX)
C
C     Rudolf Loeser, 2003 Feb 11
C---- Allocates H Ly lines background absorber data block.
C     !DASH
      save
C     !DASH
      integer IN, JXNCS, KD, MUX, N, NLY
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (KZQ(184),NLY  )
      equivalence (KZQ(189),JXNCS)
      equivalence (QZQ(  2),QELSM)
C
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     .
      equivalence
     $(ILB( 1),ILYWN),(ILB( 2),ILYEN),(ILB( 3),ILYDN),(ILB( 4),ILYAN),
     $(ILB( 5),ILYPN),(ILB( 6),ILYAA),(ILB( 7),ILYXX),(ILB( 8),ILYSA),
     $(ILB( 9),ILYSF),(ILB(10),ILYST),(ILB(11),ILYTN),(ILB(12),ILYP ),
     $(ILB(13),ILYPL),(ILB(14),ILYUL),(ILB(15),ILYVL),(ILB(16),ILYXL),
     $(ILB(17),ILYVC),(ILB(18),ILYEU)
C     .
C     !DASH
C     !EJECT
      external  HI, BYE
C
      dimension IN(*)
C
      call HI ('STROPHE')
C     !BEG
      INDPTH = (JXNCS.gt.0).and.(QELSM(:3).eq.'H  ')
      if(INDPTH) then
        KD = NLL*N
      else
        KD = NLL
      end if
C
      IN( 1) = 1
C
      IN( 2) = IN( 1)+NLL
      IN( 3) = IN( 2)+NLL
      IN( 4) = IN( 3)+NLL
      IN( 5) = IN( 4)+KD
      IN( 6) = IN( 5)+NLL
      IN( 7) = IN( 6)+NLL
      IN( 8) = IN( 7)+NLL
      IN( 9) = IN( 8)+KD
      IN(10) = IN( 9)+NLL
      IN(11) = IN(10)+NLL
C
      IN(12) = IN(11)+NLL
      IN(13) = IN(12)+NLY*N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      MUX    = IN(18)+N
C     !END
      call BYE ('STROPHE')
C
      return
      end
