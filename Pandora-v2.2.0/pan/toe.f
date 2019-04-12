      subroutine TOE
     $(X,IX,W,IW,XCBL)
C     Rudolf Loeser, 1980 Aug 14
C---- Drives line background Continuum Blocks initialization.
C     (This is version 4 of TOE.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL
      integer IN, IPEX, IS, IW, IX, IXLB1, JSTCN, LUEO, MOX, NLFDB,
     $        NOION
      logical NEEDED
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
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 18),IPEX )
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
      equivalence (LEST(35),NLFDB)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAG, EUDOXIA, MESHED, MASHED, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXLB1 )
C     !EJECT
C
      call HI ('TOE')
C     !BEG
      if(NOION.le.0) then
C
        NEEDED = ((JSTCN.le.0).and.(NLFDB.gt.0))
C
        if((IPEX.lt.0).or.(IPEX.eq.18)) then
          call MESHED  ('TOE', 2)
          write (LUEO,100) JSTCN,NLFDB,NEEDED
  100     format(' ','JSTCN =',I10,', NLFDB =',I10,', NEEDED =',L6)
          call MASHED  ('TOE')
        end if
C
        if(NEEDED) then
C         (Get W allotment)
          call NAG     (IN, IS, MOX, 'TOE')
C
          call EUDOXIA (X, W, IW, XCBL, W(IXLB1))
C
C         (Give back W allotment)
          call WGIVE   (W, 'TOE')
        end if
C
      end if
C     !END
      call BYE ('TOE')
C
      return
      end
