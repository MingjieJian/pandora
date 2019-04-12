      subroutine SCARAB
     $(AN,AD,AQ)
C
C     Rudolf Loeser, 1977 Dec 13
C---- Makes a quotient for iterative summaries.
C     !DASH
      save
C     !DASH
      real*8 AAQ, AD, AN, AQ, R1, R2, R3, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic abs, sign
C
      data      R1, R2, R3 /1.D4, 1.D-5, 9.9999999D3/
C
      call HI ('SCARAB')
C     !BEG
      if(AD.eq.ZERO) then
        AQ = ZERO
      else
        AQ  = AN/AD
        AAQ = abs(AQ)
        if(AAQ.ge.R1) then
          AQ = sign(R3,AQ)
        else if(AAQ.le.R2) then
          AQ = ZERO
        end if
      end if
C     !END
      call BYE ('SCARAB')
C
      return
      end
