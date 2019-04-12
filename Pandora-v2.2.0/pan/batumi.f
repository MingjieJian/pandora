      subroutine BATUMI
     $(X,IX,IU,IL,TE,XNE,XNC,XNUU,XNUL,CIJ,CHIJ,NO)
C
C     Rudolf Loeser, 2006 Apr 19
C---- Provides excitation data for transition (l,u).
C     !DASH
      save
C     !DASH
      real*8 CE, CH, CHIJ, CIJ, DNU, HNUKT, SE, TE, X, XNC, XNE, XNUL,
     $       XNUU
      integer IL, IU, IX, NO
      logical DUMP
C     !DASH
      external CELERY, PROD, DIVIDE, HI, BYE
C
      dimension X(*), IX(*)
C
      data DUMP /.false./
C
      call HI ('BATUMI')
C     !BEG
      call CELERY (X, IX, IU, IL, TE, XNC, DUMP, CE)
      DNU = XNUU-XNUL
      call PROD   (TE, DNU, 1, HNUKT, SE)
      call DIVIDE (CHIJ, (XNE*SE), CH)
C
      write (NO,100) IL,IU,CE,CH,CIJ,CHIJ
  100 format(' ',3X,'(',I2,'/',I2,')',5X,1P2E14.6,5X,2E14.6)
C     !END
      call BYE ('BATUMI')
C
      return
      end
