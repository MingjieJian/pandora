      subroutine AMORPHA
     $(X,IX,IU,IL,TE,CE)
C
C     Rudolf Loeser, 2007 Mar 26
C---- Drives BONOR to compute CE(IU,IL,TE)
C     !DASH
      save
C     !DASH
      real*8 CE, TE, X
      integer IL, IU, IX, JJP
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 27),JJP  )
C     !DASH
      external BONOR, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('AMORPHA')
C     !BEG
      call BONOR (IU, IL, X(JJP), TE, CE)
C     !END
      call BYE ('AMORPHA')
C
      return
      end
