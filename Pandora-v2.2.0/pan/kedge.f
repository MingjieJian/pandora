      subroutine KEDGE
     $(IU,IL,NL,CSK,LDL,XNU)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Sets up default values of CSK(iu,il).
C     !DASH
      save
C     !DASH
      real*8 CSK, DEFAULT, XNU, ZERO
      integer I, IL, IU, JDCSK, LDL, NL
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
      equivalence (MEST(11),JDCSK)
C     !DASH
      external STYPE, HI, BYE
C
C               CSK(LDL), XNU(NSL)
      dimension CSK(*),   XNU(*)
C
      call HI ('KEDGE')
C     !BEG
      KILROY = .true.
C
      do 100 I = 1,LDL
        if(CSK(I).lt.ZERO) then
C
          if(KILROY) then
            call STYPE (XNU(IU), XNU(IL), DEFAULT)
            JDCSK  = JDCSK+1
            KILROY = .false.
          end if
C
          CSK(I) = DEFAULT
        end if
  100 continue
C     !END
      call BYE ('KEDGE')
C
      return
      end
