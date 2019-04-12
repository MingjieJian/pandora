      subroutine DUMPER
     $(X,XLM,PRNT)
C
C     Rudolf Loeser, 2002 Sep 19
C---- Tells whether Continuum dumps are allowed at this wavelength.
C     !DASH
      save
C     !DASH
      real*8 DELTA, X, XLCOW, XLM, ZERO
      integer IFLG, JJDWV, KIND, LOOK, NDV
      logical PRNT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(43),NDV)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(152),JJDWV)
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
      equivalence (REST( 4),XLCOW)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LOOKUD, COMPD, HI, BYE
C
      dimension X(*)
C
      data DELTA /1.D-8/
C     !EJECT
C
      call HI ('DUMPER')
C     !BEG
      PRNT = NDV.gt.0
C
      if(PRNT) then
        call LOOKUD (X(JJDWV), NDV, DELTA, XLM, KIND, LOOK)
        PRNT = LOOK.eq.1
        if(.not.PRNT) then
          PRNT = X(JJDWV).eq.ZERO
        end if
      end if
C
      if(.not.PRNT) then
C       (XLCOW is set by STOMP)
        call COMPD  (XLCOW, XLM, DELTA, IFLG)
        PRNT = IFLG.eq.0
      end if
C     !END
      call BYE ('DUMPER')
C
      return
      end
