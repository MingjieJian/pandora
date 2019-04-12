      subroutine BARBIE
     $(J,TE,HNUKT,EXT,POP,BD,H,TERM)
C
C     Rudolf Loeser, 1988 May 10
C---- Dumps, for EVAN.
C     (This is version 2 of BARBIE.)
C     !DASH
      save
C     !DASH
      real*8 BD, EXT, H, HNUKT, POP, TE, TERM
      integer J, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external HI, BYE
C
      call HI ('BARBIE')
C     !BEG
      write (LUEO,100) J,POP,BD,TE,HNUKT,EXT,H,TERM
  100 format(' ',I2,1P2E14.6,E12.5,4E16.7)
C     !END
      call BYE ('BARBIE')
C
      return
      end
