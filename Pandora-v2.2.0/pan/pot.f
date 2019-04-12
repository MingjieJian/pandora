      subroutine POT
     $(KASE,A,B,C)
C
C     Rudolf Loeser, 1974 Oct 29
C---- Determines KASE, for ORCHID:
C
C     KASE=1 - monotonic;  KASE=2 - stable;  KASE=3 - oscillatory.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, HALF
      integer KASE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
      call HI ('POT')
C     !BEG
      if(A.lt.B) then
        if(B.lt.C) then
          KASE = 1
        else
          D = HALF*(B+A)
          if(C.lt.D) then
            KASE = 3
          else
            KASE = 2
          end if
        end if
C
      else if(A.gt.B) then
        if(B.lt.C) then
          D = HALF*(A+B)
          if(C.le.D) then
            KASE = 2
          else
            KASE = 3
          end if
        else
          KASE = 1
        end if
C
      else
        KASE = 2
      end if
C     !END
      call BYE ('POT')
C
      return
      end
