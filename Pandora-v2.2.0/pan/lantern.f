      subroutine LANTERN
     $(DUMP,CALLER,LINE,KAR,JLEV,RNUP,MRPP)
C
C     Rudolf Loeser, 2002 Jan 17
C---- Sets up dump, for MANUEL.
C     (This is version 3 of LANTERN.)
C     !DASH
      save
C     !DASH
      real*8 RNUP
      integer I, IPEX, JLEV, KAR, LUEO, MRPP
      logical DUMP
      character BLANK*1, CALLER*(*), LINE*(*)
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external MESHED, HI, BYE
C
C               RNUP(MRX+1)
      dimension RNUP(*)
C     !EJECT
C
      call HI ('LANTERN')
C     !BEG
      DUMP = (IPEX.lt.0).or.(IPEX.eq.12)
C
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100) JLEV
  100   format(' ','Level',I3,5X,'Start')
        write (LUEO,101) (I,RNUP(I),I=1,MRPP)
  101   format(' ',I8,F12.6,I8,F12.6,I8,F12.6,I8,F12.6,I8,F12.6,
     $             I8,F12.6)
C
        KAR  = 0
        LINE = BLANK
      end if
C     !END
      call BYE ('LANTERN')
C
      return
      end
