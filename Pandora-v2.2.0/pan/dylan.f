      subroutine DYLAN
     $(X,W,GD,HEAT,HEATINT)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Controls final heating rates calculation.
C     !DASH
      save
C     !DASH
      real*8 GD, HEAT, HEATINT, W, X, XNUK
      integer ICHL, ICHT, IN, IQCCI, IQSCD, IS, IVEC, JJBDI, JJCIJ,
     $        JJCK, JJFRS, JJTE, JJXND, JJXNU, JJZ, LU, LUCR, MOX, N,
     $        NL, NO, NOION
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 31),JJCK )
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(138),JJFRS)
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
      equivalence (RZQ(  9),XNUK )
      equivalence (KZQ( 94),NOION)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS(10),LUCR )
C     !EJECT
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
      equivalence (IQQ(134),IQSCD)
      equivalence (IQQ(140),IQCCI)
C     !DASH
      external IRFON, LESLIE, FRISIA, GOVAN, GRATIAN, MODRON, HOWEL,
     $         ZEUS, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               HEAT(N), HEATINT(N), GD(N)
      dimension HEAT(*), HEATINT(*), GD(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ICHT  ),(IN( 2),ICHL  ),(IN( 3),IVEC  )
C     !EJECT
C
      call HI ('DYLAN')
C     !BEG
      if(NOION.le.0) then
C       (Get, and allocate, W allotment)
        call IRFON     (IN, IS, MOX, 'DYLAN')
C
C----   Heating rates for bound-free transitions
        call LESLIE    (N, NL, XNUK, X(JJXNU), X(JJXND), X(JJCK),
     $                  X(JJBDI), W(ICHL))
C----   Heating rates for bound-bound transitions
        call FRISIA    (N, NL, X(JJXNU), X(JJXND), X(JJBDI), X(JJCIJ),
     $                  W(ICHT))
C----   Total rate
        call GRATIAN   (N, NL, W(ICHL), W(ICHT), HEAT)
C----   Print
        call GOVAN     (NO, N, NL, X(JJZ), X(JJTE), GD, W(ICHL),
     $                  W(ICHT), 1, HEAT)
C----   Save for off-line plotting
        call ZEUS      (LUCR, IQSCD, LU)
        call MODRON    (LU, 1, N, NL, W(ICHT), W(ICHL), HEAT)
C----   Integrated rates
        if(IQCCI.gt.0) then
          call HOWEL   (N, NL, X(JJZ), W(ICHT), W(ICHL), W(IVEC),
     $                  X(JJFRS))
          call GRATIAN (N, NL, W(ICHL), W(ICHT), HEATINT)
          call GOVAN   (NO, N, NL, X(JJZ), X(JJTE), GD, W(ICHL),
     $                  W(ICHT), 2, HEATINT)
          call MODRON  (LU, 2, N, NL, W(ICHT), W(ICHL), HEATINT)
        end if
C
C       (Give back W allotment)
        call WGIVE     (W, 'DYLAN')
      end if
C     !END
      call BYE ('DYLAN')
C
      return
      end
