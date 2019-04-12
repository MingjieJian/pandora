      subroutine BERNICE
     $(I1,I2,XNU,XNUC,TE,XLIM)
C
C     Rudolf Loeser, 2006 Apr 07
C---- Computes XLIM for collisions with Hydrogen.
C     (This is version 6 of BERNICE.)
C     !DASH
      save
C     !DASH
      real*8 DNU, TE, XLIM, XNU, XNUC
      integer I1, I2, MODE
C     !DASH
      external  HUNK, HI, BYE
      intrinsic abs
C
C               XNU(NSL), XNUC(NSL)
      dimension XNU(*),   XNUC(*)
C
      data MODE /1/
C
      call HI ('BERNICE')
C     !BEG
      if(I2.le.0) then
        DNU = abs(XNUC(I1)-XNU(I1))
      else
        DNU = XNU(I1)-XNU(I2)
      end if
      call HUNK (TE, DNU, MODE, XLIM)
C     !END
      call BYE ('BERNICE')
C
      return
      end
