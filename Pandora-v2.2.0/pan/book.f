      subroutine BOOK
     $(A1,AN,DZ,ZQ)
C
C     Rudolf Loeser, 1985 Feb 08
C---- Computes ZQ, for PLAUTUS.
C     !DASH
      save
C     !DASH
      real*8 A1, AN, DZ, ZERO, ZQ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
      call HI ('BOOK')
C     !BEG
      if(A1.lt.ZERO) then
C
        if(AN.le.ZERO) then
C
          if(AN.le.DZ*A1) then
            ZQ = ZERO
          else
            ZQ = AN-DZ*A1
          end if
C
        else
C
          if(AN.le.-A1) then
            ZQ = AN-DZ*A1
          else
            ZQ = A1-DZ*AN
          end if
C
        end if
      else
C
        if(A1.lt.DZ*AN) then
          ZQ = A1-DZ*AN
        else
          ZQ = ZERO
        end if
C
      end if
C     !END
      call BYE ('BOOK')
C
      return
      end
