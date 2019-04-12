      subroutine HIERON
     $(X,IX)
C
C     Rudolf Loeser, 2003 Jun 17
C---- Prints transition-related input tables.
C     (This is version 2 of HIERON.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQIJR, IX, JJAW, JJFCE, JJKIJ, JJQHI, JJRHO, JJYBR, N, NL,
     $        NO, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(242),JJAW )
      equivalence (IZOQ(248),JJFCE)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
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
      equivalence (IQQ( 88),IQIJR)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external PRIAM, LINER, CRONZI, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('HIERON')
C     !BEG
      call PRIAM    (NO, 'RHO, JBAR ...', 13)
      call LINER    (1, NO)
      write (NO,100)
  100 format(' ','Output controlled by option INTRPRNT.')
      if(IQIJR.gt.0) then
        call CRONZI (NO, N, NL, NT, X(JJYBR), X(JJRHO), X(JJQHI),
     $               X(JJAW), X(JJFCE), IX(JJKIJ))
      end if
C     !END
      call BYE ('HIERON')
C
      return
      end
