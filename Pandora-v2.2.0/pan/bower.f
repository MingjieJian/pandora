      subroutine BOWER
     $(IU,IL,NL,CVW,LDL,XNU)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Sets up default values of CVW(iu,il).
C     !DASH
      save
C     !DASH
      real*8 CVW, DEFAULT, XNU, ZERO
      integer I, IL, IU, JDCVW, LDL, NL
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(10),JDCVW)
C     !DASH
      external FLUKE, HI, BYE
C
C               CVW(LDL), XNU(NSL)
      dimension CVW(*),   XNU(*)
C
      call HI ('BOWER')
C     !BEG
      KILROY = .true.
C
      do 100 I = 1,LDL
        if(CVW(I).lt.ZERO) then
C
          if(KILROY) then
            call FLUKE (XNU(IU), XNU(IL), DEFAULT)
            JDCVW  = JDCVW+1
            KILROY = .false.
          end if
C
          CVW(I) = DEFAULT
        end if
  100 continue
C     !END
      call BYE ('BOWER')
C
      return
      end
