      subroutine DUNKER
     $(XNU,IU,IL,DNU)
C
C     Rudolf Loeser, 2004 Apr 22
C---- Computes DNU for GALIUM (really! this is how it goes.)
C     !DASH
      save
C     !DASH
      real*8 DNU, XNU
      integer IL, IU
C     !DASH
      external HI, BYE
C
C               XNU(NSL)
      dimension XNU(*)
C
      call HI ('DUNKER')
C     !BEG
      DNU = XNU(IU)-XNU(IL)
C     !END
      call BYE ('DUNKER')
C
      return
      end
