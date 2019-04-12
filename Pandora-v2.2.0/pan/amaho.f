      subroutine AMAHO
     $(KOMPO,NAB,KWC)
C
C     Rudolf Loeser, 1998 Jun 01
C---- Opens "composite" opacity data file, and reads KWC,
C     the number of wavelength values on it.
C     !DASH
      save
C     !DASH
      integer KOMPO, KWC, LUEO, NAB
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LUCK, HALT, HI, BYE
C
      call HI ('AMAHO')
C     !BEG
      KWC = 0
      if(NAB.gt.0) then
        call LUCK (KOMPO,LUEO)
C
        rewind KOMPO
        read (KOMPO,100,end=101) KWC
  100   format(4I20)
      end if
      goto 103
C
  101 continue
      write (MSSLIN(1),102)
  102 format('The "composite" opacity data file is empty.')
      call HALT   ('AMAHO', 1)
C
  103 continue
C     !END
      call BYE ('AMAHO')
C
      return
      end
