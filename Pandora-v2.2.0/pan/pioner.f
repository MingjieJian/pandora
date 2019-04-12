      subroutine PIONER
     $(BTIT,MSH,SLTIT,PWAVE)
C
C     Rudolf Loeser, 2002 Aug 22
C---- Adds a supplementary header into the current block, for REPION.
C     !DASH
      save
C     !DASH
      real*8 BTIT, PWAVE, SLTIT
      integer MSH
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
C               BTIT(100)
      dimension BTIT(*)
C
      call HI ('PIONER')
C     !BEG
      if(MSH.ge.100) then
        write (MSSLIN(1),100) PWAVE,MSH
  100   format('PWAVE =',1PE20.12,', MSH =',I12,', which equals or ',
     $         'exceeds the limit 100.')
        call HALT ('PIONER', 1)
      end if
C
      MSH = MSH+1
      BTIT(MSH) = SLTIT
C     !END
      call BYE ('PIONER')
C
      return
      end
