      subroutine KXLAB
     $(NO,XL,XH)
C
C     Rudolf Loeser, 2006 May 09
C---- Part of the K-type printer plot package.
C     !DASH
      save
C     !DASH
      real*8 XH, XL
      real*4 XHS, XLS
      integer NO
C     !DASH
      external SXLAB, HI, BYE
C
      call HI ('KXLAB')
C     !BEG
      if(NO.gt.0) then
        XLS = XL
        XHS = XH
        call SXLAB (NO, XLS, XHS)
      end if
C     !END
      call BYE ('KXLAB')
C
      return
      end
