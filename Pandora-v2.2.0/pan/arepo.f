      subroutine AREPO
     $(ARR,M,CONST)
C
C     Rudolf Loeser, 2006 Apr 11
C---- Edits an array to be printed, for TOAD.
C     !DASH
      save
C     !DASH
      real*8 ARR, CONST, DELTA, ZERO
      integer I, IFLG, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external COMPD, HI, BYE
C
C               ARR(M)
      dimension ARR(*)
C
      data DELTA /1.D-14/
C
      call HI ('AREPO')
C     !BEG
      do 100 I = 2,M
        call COMPD (ARR(I), CONST, DELTA, IFLG)
        if(IFLG.eq.0) then
          ARR(I) = ZERO
        end if
  100 continue
C     !END
      call BYE ('AREPO')
C
      return
      end
