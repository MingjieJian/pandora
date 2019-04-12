      subroutine STENTOR
     $(FX,N,LABEL)
C
C     Rudolf Loeser, 1983 Sep 27
C---- Checks FX before it is used by "XIONLY" (i.e. emergent Intensity
C     calculation).
C     !DASH
      save
C     !DASH
      real*8 FX
      integer LLT, N, NO
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external MINUSD, MESHED, VECOUT, MASHED, HI, BYE
C
C               FX(N)
      dimension FX(*)
C
      call HI ('STENTOR')
C     !BEG
      call MINUSD   (FX, 1, N, LLT)
C
      if(LLT.gt.0) then
        call MESHED ('STENTOR', 3)
        write (NO,100) LABEL
  100   format(' ','There are negative values in the Opacity table for:'/
     $         ' ',A)
        call VECOUT (NO, FX, N, 'FX')
        call MASHED ('STENTOR')
      end if
C     !END
      call BYE ('STENTOR')
C
      return
      end
