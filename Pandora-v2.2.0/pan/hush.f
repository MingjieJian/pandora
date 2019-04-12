      subroutine HUSH
C
C     Rudolf Loeser, 1981 Aug 13
C---- Writes banner pages to the output file.
C     !DASH
      save
C     !DASH
      integer I, KARB, KODE, LIM, NARB, NO
      character BLANK*1, HED*65, UHD*65
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 96),KARB )
      equivalence (KZQ(140),NARB )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  force_upper, HECTOR, HI, BYE
      intrinsic min,max
C     !EJECT
C
      call HI ('HUSH')
C     !BEG
      LIM = min(max(NARB,0),2)
      if(LIM.gt.0) then
C----   Make banner pages
C
        KODE = 0
        if(KARB.lt.0) then
          KODE = -1
        end if
C
C----   Edit Heading
        HED  = HEAD(1:65)
        if((HED(1:1).eq.'[').or.(HED(1:1).eq.'>')) then
          HED(1:13) = HED(3:13)
        end if
        HED(65:) = BLANK
        call force_upper (HED,UHD)
C
        do 100 I = 1,LIM
          call HECTOR    (UHD,65,13,KODE,NO,'1')
  100   continue
      end if
C     !END
      call BYE ('HUSH')
C
      return
      end
