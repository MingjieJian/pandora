      subroutine SIERRA
     $(X,IX,JLEV,IQRK,IQRL)
C
C     Rudolf Loeser, 1989 Jul 31
C---- Computes IQRK and IQRL for Level JLEV.
C     IQRK=1 means: compute RKI(JLEV), =0 means: do not;
C     IQRL=1 means: compute RLI(JLEV), =0 means: do not.
C     (This is version 5 of SIERRA.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQOTL, IQRK, IQRL, IX, JJRK, JJRKS, JJRL, JJRLS, JLEV
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 18),JJRL )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  4),JJRKS)
      equivalence (JZOQ(  5),JJRLS)
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
      equivalence (IQQ(344),IQOTL)
C     !DASH
C     !EJECT
      external IODINE, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('SIERRA')
C     !BEG
      if(IQOTL.gt.0) then
C----   Optically-thin-limit approximation:
C       RKI must =0 (c.f. FROLIC) and should not be recomputed
        IQRK = 0
      else
        call IODINE (IX(JJRKS), X(JJRK), JLEV, IQRK)
      end if
      call IODINE   (IX(JJRLS), X(JJRL), JLEV, IQRL)
C     !END
      call BYE ('SIERRA')
C
      return
      end
