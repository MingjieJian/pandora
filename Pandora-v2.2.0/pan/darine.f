      subroutine DARINE
     $(ITYPE)
C
C     Rudolf Loeser, 1991 Jul 08
C---- Sets ITYPE=1 if there is LSF or profile printout for the current
C     transition, =19 if not, for GOLF.
C     !DASH
      save
C     !DASH
      integer ITYPE
      logical PRNT
C     !DASH
      external DINAR,HI, BYE
C
      call HI ('DARINE')
C     !BEG
      call DINAR (PRNT)
      if(PRNT) then
        ITYPE = 1
      else
        ITYPE = 19
      end if
C     !END
      call BYE ('DARINE')
C
      return
      end
