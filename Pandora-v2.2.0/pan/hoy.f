      subroutine HOY
     $(WRATH,MR,WRAT,RRCP)
C
C     Rudolf Loeser, 2006 Jul 28
C---- Computes default RRCP values (i.e. 1/nu**3),
C     for the current level.
C     !DASH
      save
C     !DASH
      real*8 ONE, OWRATH, RRCP, WRAT, WRATH
      integer I, MR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               WRAT(MR), RRCP(MR)
      dimension WRAT(*),  RRCP(*)
C
      call HI ('HOY')
C     !BEG
      call DIVIDE (ONE, WRATH, OWRATH)
      do 100 I = 1,MR
        RRCP(I) = (WRAT(I)*OWRATH)**3
  100 continue
C     !END
      call BYE ('HOY')
C
      return
      end
