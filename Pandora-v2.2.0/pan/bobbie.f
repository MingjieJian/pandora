      subroutine BOBBIE
     $(J,TE,HNUKT,EXT,BD,XLMTHR,PILEVL,H,XNUM,XDEN)
C
C     Rudolf Loeser, 1988 May 10
C---- Dumps, for ROSE.
C     (This is version 2 of BOBBIE.)
C     !DASH
      save
C     !DASH
      real*8 BD, EXT, H, HNUKT, PILEVL, TE, XDEN, XLMTHR, XNUM
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
      call HI ('BOBBIE')
C     !BEG
      write (LUEO,100) J,BD,TE,HNUKT,EXT,H,XLMTHR,PILEVL,XNUM,XDEN
  100 format(' ',I2,1P5E12.5,4E16.7)
C     !END
      call BYE ('BOBBIE')
C
      return
      end
