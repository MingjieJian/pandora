      subroutine HEEL
     $(X,Y,YSML,YBIG,YM,YU,IMAGE,SYM,KODE,LINC)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Enters a plot point, for SEED.
C     !DASH
      save
C     !DASH
      real*8 X, Y, YBIG, YL, YM, YSML, YU, YY, ZERO
      integer KODE, LINC
      character IMAGE*(*), SYM*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HEED, FEED, LINK, HI, BYE
      intrinsic abs
C
      call HI ('HEEL')
C     !BEG
      KODE = 0
      if(Y.ne.ZERO) then
        YL = log10(abs(Y))
        if((YL.ge.YSML).and.(YL.le.YBIG)) then
C
          if(Y.lt.ZERO) then
            call HEED (YL, YSML, YBIG, YM, YU, YY)
          else
            call FEED (YL, YSML, YBIG, YM, YU, YY)
          end if
C
          call LINK   (IMAGE, X, YY, SYM, LINC)
          KODE = 1
C
        end if
      end if
C     !END
      call BYE ('HEEL')
C
      return
      end
