      subroutine KUPHAR
     $(LU,LODCG,JQ,NODCG,JX)
C
C     Rudolf Loeser, 1990 Apr 25
C---- Prints diffusion graphs control data.
C     !DASH
      save
C     !DASH
      integer JQ, JX, LODCG, LU, NODCG
C     !DASH
      external LINER, HI, BYE
C
      call HI ('KUPHAR')
C     !BEG
      if(LU.gt.0) then
        call LINER  (2, LU)
        write (LU,100) JQ,LODCG,JX,NODCG
  100   format(' ','*****   For the following "diffusion plots":'/
     $         ' ','[ JQ =',I4,' (LODCG=',I5,'); JX =',I4,' (NODCG=',I5,
     $             '); for no plots, set NODCG = 0. ]')
      end if
C     !END
      call BYE ('KUPHAR')
C
      return
      end
