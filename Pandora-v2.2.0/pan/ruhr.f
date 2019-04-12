      subroutine RUHR
     $(ITAU)
C
C     Rudolf Loeser, 1988 Jul 22
C---- Prints dump headings, for RHEIN.
C     !DASH
      save
C     !DASH
      integer ITAU, LUEO
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
      call HI ('RUHR')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,101) ITAU
  101 format(' ','*************** For depth #',I4)
C     !END
      call BYE ('RUHR')
C
      return
      end
