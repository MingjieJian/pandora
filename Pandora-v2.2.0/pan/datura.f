      subroutine DATURA
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for additional photoionization continuum calculation.
C     (This is version 2 of DATURA.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IW, IWS, IX, JJLRJ, JJTKR, JJYKR, JN, JOPAC, JOPAT, JSTCN,
     $        MUX, NL, NOION, NSH
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(102),JJTKR)
      equivalence (IZOQ(103),JJYKR)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  3),JJLRJ)
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
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
C     !DASH
C     !EJECT
      external MORTAIN, LODGE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), SWAVE(Konwal), SLTIT(Konwal)
      dimension XCBL(*),      SWAVE(*),      SLTIT(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC ),(JN( 2),JOPAT )
C
      call HI ('DATURA')
C     !BEG
      DOIT = (NOION.le.0).and.(JSTCN.le.0)
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'DATURA')
C
        call LODGE   (X, XCBL, IX(JJLRJ), X(JJTKR), X(JJYKR),
     $                IW(JOPAC), IW(JOPAT), SWAVE, SLTIT, NSH)
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'DATURA')
      end if
C     !END
      call BYE ('DATURA')
C
      return
      end
