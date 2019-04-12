      subroutine GRIDLY
     $(IMAGE,CL,CU,DC,A,B,SYM,MODE)
C
C     Rudolf Loeser, 1970 Jan 28
C---- Enters grid lines.
C     Inserts lines for all values between CL and CU
C     which are integral multiples of DC.
C     MODE=1: X constant; =2: Y constant.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CL, CU, DC, ZERO, ZL, ZLDC, ZU
      integer IZLDC, MODE
      character IMAGE*(*), SYM*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  KLINEC, HI, BYE
      intrinsic max,min
C
      call HI ('GRIDLY')
C     !BEG
      ZL    = min(CL,CU)
      ZU    = max(CL,CU)
      IZLDC = ZL/DC
      ZLDC  = IZLDC
      C     = DC*ZLDC
      if((C.ne.ZL).and.(ZL.lt.ZERO)) then
        C = C-DC
      end if
C
  100 continue
        C = C+DC
        if(C.lt.ZU) then
          if(MODE.eq.1) then
            call KLINEC (IMAGE,C,A,C,B,SYM,0)
            goto 100
          else
            call KLINEC (IMAGE,A,C,B,C,SYM,0)
            goto 100
          end if
        end if
C     !END
      call BYE ('GRIDLY')
C
      return
      end
