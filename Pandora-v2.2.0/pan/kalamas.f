      subroutine KALAMAS
     $(IU,IL,NL,CVW,LDL,XLM)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Sets up default value of CVW(iu,il), for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 CVW, DEFAULT, XLM, ZERO
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
      external KELKIT, HI, BYE
C
C               CVW(LDL)
      dimension CVW(*)
C
      call HI ('KALAMAS')
C     !BEG
      KILROY = .true.
C
      do 100 I = 1,LDL
        if(CVW(I).lt.ZERO) then
          if(KILROY) then
            KILROY = .false.
            call KELKIT (IU,IL,DEFAULT,XLM)
            JDCVW = JDCVW+1
          end if
          CVW(I) = DEFAULT
        end if
  100 continue
C     !END
      call BYE ('KALAMAS')
C
      return
      end
