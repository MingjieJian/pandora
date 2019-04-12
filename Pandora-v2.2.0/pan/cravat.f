      subroutine CRAVAT
     $(A,B,R,CA,CB,CD)
C
C     Rudolf Loeser, 1998 Aug 11
C---- Computes and encodes data, for TAKE.
C     !DASH
      save
C     !DASH
      real*8 A, B, R, ZERO
      character CA*15, CB*15, CD*15
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, NOTICE, HI, BYE
C
      call HI ('CRAVAT')
C     !BEG
      if(A.eq.ZERO) then
        R = ZERO
      else
        call DIVIDE (A,B,R)
      end if
C
      call NOTICE   (15,A,B, CA,CB,CD)
C     !END
      call BYE ('CRAVAT')
C
      return
      end
