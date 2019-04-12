      subroutine ZAGEN
     $(DUMP,L,CDL,I)
C
C     Rudolf Loeser, 2005 Feb 16
C---- Debug output for GAFFE.
C     !DASH
      save
C     !DASH
      real*8 CDL
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
      call HI ('ZAGEN')
C     !BEG
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) L,CDL,I
  100   format(' ','Line component #',I5,', CDL =',F8.4,'; depth #',I6)
      end if
C     !END
      call BYE ('ZAGEN')
C
      return
      end
