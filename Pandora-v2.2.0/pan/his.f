      subroutine HIS
     $(XU,XL,VU,VL,Z,GM,XND,BD,VLS,NL,NSL)
C
C     Rudolf Loeser, 1974 Apr 03
C---- Provides additional detail printout for CHAIN.
C     !DASH
      save
C     !DASH
      real*8 BD, GM, VL, VLS, VU, XL, XND, XU, Z
      integer I, LUEO, NL, NSL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DARROUT, HI, BYE
C
C               Z(NL,NL), GM(NL), XND(NL), BD(NL)
      dimension Z(*),     GM(*),  XND(*),  BD(*)
C
      call HI ('HIS')
C     !BEG
      call DARROUT (LUEO, Z, NL, NL, 'Matrix Z')
C
      call LINER   (1, LUEO)
      write (LUEO,102)
  102 format(' ','Data for all levels K:'//
     $       ' ',4X,'K',16X,'GM',16X,'ND',16X,'BD')
      write (LUEO,103) (I,GM(I),XND(I),BD(I),I=1,NL)
  103 format(' ',I5,1P3E18.10)
C
      call LINER   (1, LUEO)
      if(NSL.gt.NL) then
        write (LUEO,104) VLS
  104   format(' ','Term added to VL for supplementary levels VLS =',
     $             1PE18.10)
      end if
      write (LUEO,105) XU,XL,VU,VL
  105 format(' ','XU =',1PE18.10,5X,'XL =',E18.10,5X,'VU =',E18.10,5X,
     $           'VL =',E18.10)
C     !END
      call BYE ('HIS')
C
      return
      end
