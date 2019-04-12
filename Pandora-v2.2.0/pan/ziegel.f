      subroutine ZIEGEL
     $(DUMP,L,CDL,I,SAP)
C
C     Rudolf Loeser, 2004 Nov 22
C---- Debug output for JAFFA.
C     !DASH
      save
C     !DASH
      real*8 CDL, SAP
      integer I, L, LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ZIEGEL')
C     !BEG
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) L,CDL,I,SAP
  100   format(' ','Line component #',I5,', CDL =',F8.4,'; depth #',
     $             I6,', SAP =',1PE14.6)
      end if
C     !END
      call BYE ('ZIEGEL')
C
      return
      end
