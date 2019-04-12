      subroutine EBISSA
     $(LU,NL,NSL,LONG,NBAD,EDITED,OFCE)
C
C     Rudolf Loeser, 1983 May 03
C---- Prints a heading, for CYMBAL.
C     !DASH
      save
C     !DASH
      integer IQANU, JBFSW, LU, NL, NSL
      logical EDITED, LONG, NBAD, OFCE
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
      equivalence (KZQ( 46),JBFSW)
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
      equivalence (IQQ(256),IQANU)
C     !DASH
      external LINER, LIED, JOY, IDLE, HI, BYE
C     !EJECT
C
      call HI ('EBISSA')
C     !BEG
      if(LU.gt.0) then
        LONG = (IQANU.le.0).or.NBAD
C
        call LIED    (LU, NBAD)
C
        if(LONG) then
          call LINER (1, LU)
          write (LU,100)
  100     format(' ','B(J) is defined such that B(J) = R(J) / R(ION) ',
     $               '(when R''s differ from zero); here, R = '
     $               '"N" / "N*".'/
     $           ' ','Values of B are computed, however, from the ',
     $               'final values of B-ratio appearing in the ',
     $               'preceding "RHO AND RBD" printout.'//
     $           ' ','Values of N are computed from the new '
     $               'weighted and edited final B-values.')
C
          call JOY   (LU, EDITED, OFCE)
C
          call LINER (1, LU)
          write (LU,101)
  101     format(' ','FION  is the fraction of ions,                '
     $               '                         FIONL is log10(FION);'/
     $           ' ','FLVS  is the fraction representing the sum of ',
     $               'bound-level populations; FLVSL is log10(FLVS).')
        end if
C
        call IDLE    (LU)
C
        if(NL.lt.NSL) then
          call LINER (1, LU)
          write (LU,102) JBFSW
  102     format(' ','JBFSW',1X,I3,11X,'Supplementary levels ',
     $               'B calculation method switch.')
        end if
      end if
C     !END
      call BYE ('EBISSA')
C
      return
      end
