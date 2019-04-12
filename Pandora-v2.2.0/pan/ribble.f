      subroutine RIBBLE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 31
C---- Controls "SPEC": line and continuous spectrum calculations.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IQPH2, IQSFO, IQSFS, IS, IW, IWLY, IX, IXLY, JSTCN,
     $        LYNC, MOX, NOION
      logical LEGEND, PLANE, SOMEC, SOMEL, SPHERE
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
      equivalence (IQQ(  5),IQPH2)
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(  8),IQSFO)
C     !DASH
C     !EJECT
      external MEIG, AVON, STOUR, SWALE, LOX, EDEN, RUTE, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IWLY  ),(IN( 2),IXLY  )
C
      call HI ('RIBBLE')
C     !BEG
      if(IQPH2.gt.0) then
        SPHERE = (IQSFS.gt.0).or.(IQSFO.gt.0)
        PLANE  = .not.SPHERE
        LEGEND = .true.
        SOMEC  = .false.
        SOMEL  = .false.
C
C----   Initialize spectrum summary data collection process
        call LOX    (PLANE)
C----   Initialize special spectrum save file
        call MEIG   (X, IX, W, IW)
C
C       (GET, and allocate, W Allotment)
        call RUTE   (IN, IS, MOX, 'RIBBLE')
C
C----   Compute emergent continuum
        call STOUR  (X, IX, W, IW, SPHERE, SOMEC, LEGEND, LYNC,
     $               W(IWLY), W(IXLY))
C
        if((NOION.le.0).and.(JSTCN.eq.0)) then
C----     Compute line profiles
          call AVON (X, IX, W, IW, SPHERE, SOMEL, LEGEND, LYNC,
     $               W(IWLY), W(IXLY))
        end if
C
C       (Give back W allotment)
        call WGIVE  (W, 'RIBBLE')
C
C----   Do spectrum summary
        call SWALE  (X, IX, W, IW, PLANE, SOMEL, SOMEC)
C
C----   Do contributions summary
        call EDEN   (X, IX, W, IW, PLANE, SOMEC)
      end if
C     !END
      call BYE ('RIBBLE')
C
      return
      end
