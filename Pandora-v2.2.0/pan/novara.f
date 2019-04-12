      subroutine NOVARA
     $(IU,IL,NL,CSK,LDL,KHSSW,PMSK)
C
C     Rudolf Loeser, 1991 May 13
C---- Sets up default values of CSK(iu,il), for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 CSK, DEFAULT, PMSK, ZERO
      integer I, IL, IU, JDCSK, KHSSW, LDL, NL
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
      external NERONE, HI, BYE
C
C               CSK(LDL)
      dimension CSK(*)
C
      call HI ('NOVARA')
C     !BEG
      KILROY = .true.
      do 100 I = 1,LDL
C
        if(CSK(I).lt.ZERO) then
C
          if(KILROY) then
            KILROY = .false.
            if(KHSSW.eq.1) then
              DEFAULT = ZERO
            else
              call NERONE (IU, IL, PMSK, DEFAULT)
              JDCSK = JDCSK+1
            end if
          end if
C
          CSK(I) = DEFAULT
        end if
C
  100 continue
C     !END
      call BYE ('NOVARA')
C
      return
      end
