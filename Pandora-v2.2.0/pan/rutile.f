      subroutine RUTILE
     $(NV,V,VL,G,FINJ,FINJL,SFINJ)
C
C     Rudolf Loeser, 1984 Jul 06
C---- Prints, for FINCH.
C     !DASH
      save
C     !DASH
      real*8 FINJ, FINJL, G, SFINJ, V, VL
      integer I, LUEO, NV
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
C               V(NV), VL(NV), G(NV), FINJ(NV), FINJL(NV)
      dimension V(*),  VL(*),  G(*),  FINJ(*),  FINJL(*)
C
      call HI ('RUTILE')
C     !BEG
      write (LUEO, 100)
  100 format(' ',19X,'V',10X,'log(V)',15X,'G',12X,'FINJ',7X,
     $           'log(FINJ)')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,V(I),VL(I),G(I),FINJ(I),FINJL(I),I=1,NV)
  101 format(5(' ',I4,1P5E16.8/))
C
      call LINER (1, LUEO)
      write (LUEO,102) SFINJ
  102 format(' ',44X,'Integral',1PE16.8)
C     !END
      call BYE ('RUTILE')
C
      return
      end
