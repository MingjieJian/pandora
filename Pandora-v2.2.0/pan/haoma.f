      subroutine HAOMA
     $(LUKA,KODE,NW)
C
C     Rudolf Loeser, 1983 Jun 30
C---- Opens "averaged" opacity data file, and reads NW,
C     the number of wavelength values in it.
C     !DASH
      save
C     !DASH
      integer KODE, LUEO, LUKA, NW
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
      external PUCK, HALT, HI, BYE
C
      call HI ('HAOMA')
C     !BEG
      NW = 0
      if(KODE.gt.0) then
        call PUCK (LUKA, LUEO)
C
        rewind LUKA
        read (LUKA,end=100) NW
      end if
      goto 102
C
  100 continue
      write (MSSLIN(1),101)
  101 format('The "averaged" opacity data file is empty.')
      call HALT   ('HAOMA', 1)
C
  102 continue
C     !END
      call BYE ('HAOMA')
C
      return
      end
