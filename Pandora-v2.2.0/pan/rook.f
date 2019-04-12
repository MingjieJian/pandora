      subroutine ROOK
     $(A,B,F,C)
C
C     Rudolf Loeser, 1985 Feb 06
C---- Editing, for PLAUTUS.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, F, ZERO
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
      intrinsic min, max
C
      call HI ('ROOK')
C     !BEG
      if(A.gt.ZERO) then
        if(B.gt.ZERO) then
          if(B.gt.A) then
            C = min(B,F*A)
          else
            C = B
          end if
        else
          C  = B
        end if
C
      else
        if(B.lt.ZERO) then
          if(B.lt.A) then
            C = max(B,F*A)
          else
            C = B
          end if
        else
          C = B
        end if
      end if
C     !END
      call BYE ('ROOK')
C
      return
      end
