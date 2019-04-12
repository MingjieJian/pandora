      subroutine SPUPPA
     $(CALLER,LABEL,N,Z,VBMB,ZETA,DZZ,GX1M,GX1)
C
C     Rudolf Loeser, 2004 Jan 20
C---- Dumps for PAPPUS.
C     !DASH
      save
C     !DASH
      real*8 DZZ, GX1, GX1M, VBMB, Z, ZETA
      integer I, LUEO, N
      character CALLER*(*), LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MESHED, MASHED, HI, BYE
C
C               VBMB(N), GX1M(N), DZZ(N), GX1(N), ZETA(N), Z(N)
      dimension VBMB(*), GX1M(*), DZZ(*), GX1(*), ZETA(*), Z(*)
C
      call HI ('SPUPPA')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) LABEL
  100 format(' ','Special calculation of GXL for Level 1 of ',A//
     $       ' ',19X,'Z',16X,'VBMB',16X,'zeta',12X,'dzeta/dZ',
     $           16X,'GX1M',17X,'GX1')
      call LINER  (1, LUEO)
      write (LUEO,101) (I,Z(I),VBMB(I),ZETA(I),DZZ(I),GX1M(I),GX1(I),
     $                  I=1,N)
  101 format(5(' ',I4,1P6E20.12/))
      call MASHED (CALLER)
C     !END
      call BYE ('SPUPPA')
C
      return
      end
