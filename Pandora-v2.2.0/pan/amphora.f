      subroutine AMPHORA
     $(X,IX,IU,IL,TE,CE)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Drives NIOBE to compute CE(IU,IL,IONST,TE)
C     !DASH
      save
C     !DASH
      real*8 CE, TE, X
      integer IL, IONST, IU, IX, JJAAT, JJP, JJXNU
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
      equivalence (KZQ( 56),IONST)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(262),JJAAT)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 27),JJP  )
C     !DASH
      external AMPHION, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('AMPHORA')
C     !BEG
      call AMPHION (IU, IL, TE, X(JJAAT), X(JJP), X(JJXNU), IONST, CE)
C     !END
      call BYE ('AMPHORA')
C
      return
      end
