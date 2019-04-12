      subroutine SYLVIA
     $(NO,KT5,LASE)
C
C     Rudolf Loeser, 1996 Jan 02
C---- Writes a legend for PONG.
C     (This is version 3 of SYLVIA.)
C     !DASH
      save
C     !DASH
      integer KB, LASE, NO
      logical KT5
      character BLANK*1, DATE*11, QMODL*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  3),QMODL)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C     !DASH
      external  PRIAM, GET_DATE, LINER, HI, BYE
      intrinsic index
C     !EJECT
C
      call HI ('SYLVIA')
C     !BEG
      if(NO.gt.0) then
C
        if((LASE.eq.0).and.KT5) then
          call PRIAM    (NO, 'GAS DATA', 8)
C
        else
          call PRIAM    (NO, 'MODEL DATA', 10)
          call GET_DATE (DATE)
          KB = index(QMODL,BLANK)
C
          call LINER    (3, NO)
          write (NO,100) QMODL(:KB-1),DATE,VERSION
  100     format(' ','Atmosphere Model: ',A,', as of ',A//
     $           ' ','(PANDORA program version',F8.3,')')
        end if
        call LINER      (3, NO)
C
      end if
C     !END
      call BYE ('SYLVIA')
C
      return
      end
