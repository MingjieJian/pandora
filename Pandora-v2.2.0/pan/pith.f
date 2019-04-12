      subroutine PITH
     $(XNRR,XNRW,XNBR,XNBW,LINE)
C
C     Rudolf Loeser, 1988 Jul 29
C---- Encodes a line of scratch I/O performance data, for SAY.
C     (This is version 3 of PITH.)
C     !DASH
      save
C     !DASH
      real*8 XNBR, XNBW, XNRR, XNRW, ZERO
      character FIELD*8, LINE*38
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
C
      call HI ('PITH')
C     !BEG
      LINE = '       0       0          0          0'
C
      if(XNRR.gt.ZERO) then
        write (FIELD,100) XNRR
  100   format(F8.0)
        LINE( 2: 8) = FIELD(1:7)
        write (LINE(17:27),101) XNBR
  101   format(1PE11.3)
      end if
C
      if(XNRW.gt.ZERO) then
        write (FIELD,100) XNRW
        LINE(10:16) = FIELD(1:7)
        write (LINE(28:38),101) XNBW
      end if
C     !END
      call BYE ('PITH')
C
      return
      end
