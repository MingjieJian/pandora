      subroutine QUIXOTE
     $(W)
C
C     Rudolf Loeser, 1986 Oct 31
C---- Initializes Line Source Function data blocks.
C     (This is version 2 of QUIXOTE.)
C     !DASH
      save
C     !DASH
      real*8 W
      integer IBLOCK, IN, IQEXA, IQSFS, IS, MOX, NOION
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
      equivalence (KZQ( 94),NOION)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ( 31),IQSFS)
C     !DASH
C     !EJECT
      external ZERO1, AMURAT, MARANT, RUFFIN, ISABEL, MELINDA, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IBLOCK)
C
      call HI ('QUIXOTE')
C     !BEG
      if(NOION.le.0) then
C       (Get W allotment)
        call MELINDA    (IN, IS, MOX, 'QUIXOTE')
C
        call ZERO1      (W(IBLOCK), (MOX-IS+1))
C
        if(IQEXA.le.0) then
          call AMURAT   (W(IBLOCK))
        else
          if(IQSFS.le.0) then
            call MARANT (W(IBLOCK))
          else
            call RUFFIN (W(IBLOCK))
          end if
        end if
C
C----   Print Line Source Function Data Blocks index (if needed)
        call ISABEL
C
C       (Give back W allotment)
        call WGIVE      (W, 'QUIXOTE')
      end if
C     !END
      call BYE ('QUIXOTE')
C
      return
      end
