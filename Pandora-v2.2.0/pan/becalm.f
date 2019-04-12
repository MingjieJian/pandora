      subroutine BECALM
     $(X,IX,IU,IL,TE,HYDR,CE,OK)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Drives BEKLA to compute CE(IU,IL,TE) for radiative
C     transitions of neutral atoms.
C     !DASH
      save
C     !DASH
      real*8 CE, TE, X, ZERO
      integer IL, IONST, IU, IX, JJAIJ, JJLRQ, JJNPQ, JJP, JJXCU, JJXNU
      logical HYDR, MESS, OK
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 27),JJP  )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
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
      equivalence (KZQ( 56),IONST)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external BEKLA, HI, BYE
C
      dimension X(*), IX(*)
C
      data MESS /.false./
C
      call HI ('BECALM')
C     !BEG
      if(IONST.eq.1) then
        call BEKLA (HYDR, MESS, IU, IL, TE, X(JJAIJ), X(JJP),
     $              X(JJXNU), X(JJXCU), IX(JJNPQ), IX(JJLRQ),
     $              CE, OK)
      else
        CE = ZERO
      end if
C     !END
      call BYE ('BECALM')
C
      return
      end
