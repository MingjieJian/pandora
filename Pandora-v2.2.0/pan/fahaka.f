      subroutine FAHAKA
     $(IU,IL,NL,CRS,AIJ,XNU)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Sets up default values of CRS(iu,il) for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 AIJ, CRS, XNU, ZERO
      integer IL, IU, JDCRS, NL
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
      equivalence (MEST(12),JDCRS)
C     !DASH
      external GIZON, HI, BYE
C
C               AIJ(NL,NL), XNU(NSL)
      dimension AIJ(*),     XNU(*)
C
      call HI ('FAHAKA')
C     !BEG
      if(CRS.lt.ZERO) then
        call GIZON (IU,IL,NL,CRS,AIJ,XNU)
        JDCRS = JDCRS+1
      end if
C     !END
      call BYE ('FAHAKA')
C
      return
      end
