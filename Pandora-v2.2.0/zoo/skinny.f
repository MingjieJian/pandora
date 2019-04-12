      subroutine SKINNY
     $(ML,MR,IMX,INDX,X,DX,F,DF,FP,IM)
C
C     Rudolf Loeser, 2003 Oct 28
C---- Details dump, for SLATHER.
C     !DASH
      save
C     !DASH
      real*8 DF, DX, F, FP, X
      integer I, IM, IMX, INDX, ML, MR
      character BLANK*1, MARK*1, STAR*1
C     !DASH
      external MARKI
C
      dimension X(*), DX(*), F(*), DF(*), FP(*), IM(*)
C
      data BLANK,STAR /' ', '*'/
C
C     !BEG
      write (*,100) IMX,INDX
  100 format(/
     $       ' ','Smoothing dump for i =',I8,', near IXASM =',I8/
     $       ' ',4X,'i',19X,'X',18X,'DX',19X,'F',18X,'DF',18X,'FP',
     $           5X,'count')
      do 102 I = ML,MR
        call MARKI (I, IMX, MARK, BLANK, STAR)
        write (*,101) I,MARK,X(I),DX(I),F(I),DF(I),FP(I),IM(I)
  101   format(' ',I5,A,1PE19.11,4E20.11,I10)
  102 continue
C     !END
C
      return
      end
