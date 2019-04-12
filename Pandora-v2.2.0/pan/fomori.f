      subroutine FOMORI
     $(DL,XHZ,K,EW, WDL,WHZ,WEW)
C
C     Rudolf Loeser, 1992 Sep 25
C---- Supervises the calculation of equivalent width.
C     WDL, WHZ, and WEW are working storage.
C     (This is version 4 of FOMORI.)
C     !DASH
      save
C     !DASH
      real*8 DL, EW, WDL, WEW, WHZ, XHZ
      integer I, IZ, J, K
C     !DASH
      external  QUEBEC, PUZZLE, HI, BYE
      intrinsic abs
C
C               DL(K), XHZ(K), EW(K), WDL(K), WHZ(K), WEW(K)
      dimension DL(*), XHZ(*), EW(*), WDL(*), WHZ(*), WEW(*)
C
      call HI ('FOMORI')
C     !BEG
      call QUEBEC   (DL,K,'DL','FOMORI',IZ)
C
      if(IZ.le.1) then
        call PUZZLE (DL,XHZ,K,EW)
      else
C
        call PUZZLE (DL(IZ),XHZ(IZ),(K-IZ+1),EW(IZ))
C
        I = IZ+1
        do 100 J = 1,IZ
          I = I-1
          WDL(J) = abs(DL(I))
          WHZ(J) = XHZ(I)
  100   continue
C
        call PUZZLE (WDL,WHZ,IZ,WEW)
C
        I = IZ+1
        do 101 J = 1,(IZ-1)
          I = I-1
          EW(J) = WEW(I)
  101   continue
C
      end if
C     !END
      call BYE ('FOMORI')
C
      return
      end
