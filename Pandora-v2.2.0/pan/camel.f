      subroutine CAMEL
     $(X,XLM,KCOMP)
C
C     Rudolf Loeser, 1983 Jun 29
C---- Sets KCOMP=1 if Composite Line Opacity is defined at this
C     wavelength, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 X, XLM
      integer JJBNL, JJBNU, KCOMP, NAB, NCP, jummy
      logical ANY, MEMBER
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(45),NAB)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
C     !DASH
      external KURASH, HI, BYE
C
      dimension X(*)
C
C
      call HI ('CAMEL')
C     !BEG
      KCOMP = 0
C
      ANY = NCP.gt.0
      if(ANY) then
        call KURASH (NAB,X(JJBNL),X(JJBNU),XLM,MEMBER,jummy)
        if(MEMBER) then
          KCOMP = 1
        end if
      end if
C     !END
      call BYE ('CAMEL')
C
      return
      end
