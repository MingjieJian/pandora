      subroutine GOOD
     $(DUMP,KASE,DLJ,I,INT,LABEL,DMPI)
C
C     Rudolf Loeser, 2000 Jun 15
C---- Sets up dump paraphernalia, for LEAVES.
C     KASE = 1 for Shell; Kase = 2 for Disk.
C     (This is version 4 of GOOD.)
C     !DASH
      save
C     !DASH
      real*8 DLJ
      integer I, INT, KASE
      logical DMPI, DUMP
      character LABEL*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('GOOD')
C     !BEG
      DMPI = DUMP.and.(I.eq.INT)
C
      if(KASE.eq.1) then
        write (LABEL,100) DLJ,I
  100   format('Delta-Lambda =',1PE14.6,
     $         ', Shell ray tangent to radius #',I4)
      else if(KASE.eq.2) then
        write (LABEL,101) DLJ,I
  101   format('Delta-Lambda =',1PE14.6,
     $         ', Disk ray #',I4)
C
      else
        write (MSSLIN(1),102) KASE
  102   format('KASE =',I12,', which is not 1 or 2.')
        call HALT ('GOOD', 1)
      end if
C     !END
      call BYE ('GOOD')
C
      return
      end
