      subroutine MONKEY
     $(IMAGE,CL,CU,DC,A,B,SYM,MODE)
C
C     Rudolf Loeser, 1999 Apr 14
C---- Enters grid lines.
C     Inserts lines for all values between CL and CU
C     which are integral multiples of DC.
C     MODE=1: X constant; =2: Y constant.
C     !DASH
      save
C     !DASH
      real*8 A, B, CL, CU, DC, ZERO
      integer LUEO, MODE
      character IMAGE*(*), SYM*1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external GRIDLY, MESHED, MASHED, HI, BYE
C
      call HI ('MONKEY')
C     !BEG
      if((MODE.ge.1).and.(MODE.le.2).and.(DC.ne.ZERO)) then
        call GRIDLY (IMAGE, CL, CU, DC, A, B, SYM, MODE)
      else
        call MESHED ('MONKEY', 3)
        write (LUEO,100) MODE,DC
  100   format(' ','Trouble in MONKEY: grid lines in graph.'//
     $         ' ','MODE =',I10,' or DC =',1PE12.4,
     $             ' does not make sense.')
        call MASHED ('MONKEY')
      end if
C     !END
      call BYE ('MONKEY')
C
      return
      end
