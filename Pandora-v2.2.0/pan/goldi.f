      subroutine GOLDI
     $(N,LABEL,EMU,VEX,VP,GTU,PHI,COP,BC,SUL,FX,GX)
C
C     Rudolf Loeser, 1981 Sep 09
C---- Prints debug output for intensity calculation using spherical
C     geometry.
C     !DASH
      save
C     !DASH
      real*8 BC, COP, EMU, FX, GTU, GX, PHI, SUL, VEX, VP
      integer I, LUEO, N
      character LABEL*(*)
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
      dimension SUL(*), VEX(*), PHI(*), EMU(*), COP(*), BC(*), GTU(*),
     $          GX(*), VP(*), FX(*)
C
      call HI ('GOLDI')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) LABEL
  100 format(' ','Details of Line Intensity calculation using ',
     $           'spherical geometry.'//
     $       ' ',A//
     $       ' ',13X,'MU',9X,'VEX',7X,'V-ray',9X,'GTU',9X,'PHI',
     $           9X,'COP',10X,'BC',9X,'SUL',3X,'FX(=opac)',
     $           3X,'GX(=s.f.)')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,EMU(I),VEX(I),VP(I),GTU(I),PHI(I),COP(I),
     $                  BC(I),SUL(I),FX(I),GX(I),I=1,N)
  101 format(5(' ',I3,1P10E12.4/))
C     !END
      call BYE ('GOLDI')
C
      return
      end
