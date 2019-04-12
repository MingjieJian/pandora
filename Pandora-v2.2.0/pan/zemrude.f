      subroutine ZEMRUDE
     $(X,XLM,FMULT)
C
C     Rudolf Loeser, 1977 Apr 05
C---- Drives FUDGE to obtain opacity multiplier at wavelength XLM.
C     !DASH
      save
C     !DASH
      real*8 FMULT, X, XLM
      integer JJLMM, JJMLC, JM
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(24),JM )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 93),JJLMM)
      equivalence (IZOQ( 94),JJMLC)
C     !DASH
      external  FUDGE, HI, BYE
      intrinsic abs
C
      dimension X(*)
C
      call HI ('ZEMRUDE')
C     !BEG
      call FUDGE (abs(XLM), JM, X(JJLMM), X(JJMLC), FMULT)
C     !END
      call BYE ('ZEMRUDE')
C
      return
      end
