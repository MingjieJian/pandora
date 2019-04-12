      subroutine KOSHER
     $(WAVE,X,USE)
C
C     Rudolf Loeser, 1988 Apr 25
C---- Determines whether WAVE should be deleted from the set of
C     Continuum Wavelengths.
C     Sets USE = .false. if yes.
C     !DASH
      save
C     !DASH
      real*8 DELTA, WAVE, X
      integer JJSWV, KIND, LOOK, NWS
      logical USE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(23),NWS)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(185),JJSWV)
C     !DASH
      external LOOKUD, HI, BYE
C
      dimension X(*)
C
      data DELTA /1.D-8/
C
      call HI ('KOSHER')
C     !BEG
      if(NWS.gt.0) then
        call LOOKUD (X(JJSWV), NWS, DELTA, WAVE, KIND, LOOK)
      else
        LOOK = 2
      end if
C
      USE = LOOK.eq.2
C     !END
      call BYE ('KOSHER')
C
      return
      end
