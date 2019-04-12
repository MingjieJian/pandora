      subroutine KLISBOE
     $(KODE)
C
C     Rudolf Loeser, 1999 Jan 05
C---- Writes a marker, for OBELISK.
C     !DASH
      save
C     !DASH
      integer KODE, LUEO
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
      call HI ('KLISBOE')
C     !BEG
      if(KODE.eq.1) then
        call LINER (3, LUEO)
        write (LUEO,100) 'Start'
  100   format(' ','&&&&& ',A,' of simultaneous solution.')
        call LINER (1, LUEO)
      else
        call LINER (1, LUEO)
        write (LUEO,100) 'End'
        call LINER (3, LUEO)
      end if
C     !END
      call BYE ('KLISBOE')
C
      return
      end
