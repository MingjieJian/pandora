      subroutine PRIMROS
     $(IU,IL,NL,CRD,LDL,AIJ,XNU)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Sets up default values of CRD(iu,il).
C     (This is version 3 of PRIMROS.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CRD, DEFAULT, XNU, ZERO
      integer I, IL, IU, JDCRD, LDL, NL
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
      equivalence (MEST( 8),JDCRD)
C     !DASH
      external IRIS, HI, BYE
C
C               CRD(LDL), XNU(NSL), AIJ(NL,NL)
      dimension CRD(*),   XNU(*),   AIJ(*)
C
      call HI ('PRIMROS')
C     !BEG
      KILROY = .true.
C
      do 100 I = 1,LDL
        if(CRD(I).lt.ZERO) then
C
          if(KILROY) then
            call IRIS (IU, IL, NL, AIJ, XNU, DEFAULT)
            JDCRD  = JDCRD+1
            KILROY = .false.
          end if
C
          CRD(I) = DEFAULT
        end if
  100 continue
C     !END
      call BYE ('PRIMROS')
C
      return
      end
