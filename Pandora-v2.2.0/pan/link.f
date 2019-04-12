      subroutine LINK
     $(IMAGE,X,Y,SYM,KODE)
C
C     Rudolf Loeser, 1971 May 14
C---- Enters a line increment into a graph image.
C     (KODE is the initialization signal.)
C     !DASH
      save
C     !DASH
      real*8 X, Y
      integer IY, IYO, JX, JXO, KODE
      logical OKC, OKO, SEG
      character IMAGE*(*), SYM*1
C     !DASH
      external KWHERE, KTESTP, KPLOTP, KLINEP, HI, BYE
C
      call HI ('LINK')
C     !BEG
      if(KODE.gt.0) then
        JXO = 0
        IYO = 0
        OKO = .false.
      end if
C
      call KWHERE   (IMAGE, X, Y, JX, IY)
      call KTESTP   (IMAGE, JX, IY, OKC)
C
      if(OKC) then
        call KPLOTP (IMAGE, JX, IY, SYM)
      end if
C
      SEG = (OKC.or.OKO).and.(KODE.le.0)
      if(SEG) then
        call KLINEP (IMAGE, JXO, IYO, JX, IY, SYM, 0)
      end if
C
      KODE = 0
      JXO  = JX
      IYO  = IY
      OKO  = OKC
C     !END
      call BYE ('LINK')
C
      return
      end
