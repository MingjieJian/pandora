      subroutine GLISTEN
     $(NO,NFL,WNRMLA,FNRMLA,WNRMLB,FNRMLB)
C
C     Rudolf Loeser, 2005 Nov 03
C---- Prints background H Ly lines normalization data.
C     (This is version 4 of GLISTEN.)
C     !DASH
      save
C     !DASH
      real*8 FNRMLA, FNRMLB, WNRMLA, WNRMLB
      integer I, NFL, NO
C     !DASH
      external LINER, HI, BYE
C
C               WNRMLA(NFL), FNRMLA(NFL), WNRMLB(NFL), FNRMLB(NFL)
      dimension WNRMLA(*),   FNRMLA(*),   WNRMLB(*),   FNRMLB(*)
C
      call HI ('GLISTEN')
C     !BEG
      if(NO.gt.0) then
        call LINER (2, NO)
        write (NO,100)
  100   format(' ','Background H Ly lines normalization factors ',
     $             '(fixed tables of wavelengths).'/
     $         ' ','(Printed because option ULNORM is ON.)'//
     $         ' ',11X,'WNRMLA',6X,'FNRMLA',6X,'WNRMLB',6X,'FNRMLB')
        call LINER (1, NO)
        write (NO,101) (I,WNRMLA(I),FNRMLA(I),WNRMLB(I),FNRMLB(I),
     $                  I=1,NFL)
  101   format(5(' ',I5,0PF12.2,1PE12.4,0PF12.2,1PE12.4/))
      end if
C     !END
      call BYE ('GLISTEN')
C
      return
      end
