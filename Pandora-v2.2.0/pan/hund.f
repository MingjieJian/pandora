      subroutine HUND
     $(X,IX,J,TE,XNE,XNC,XNU,XNUC,CKI,CHKI,NO)
C
C     Rudolf Loeser, 2006 Apr 18
C---- Provides ionization data for level J.
C     (This is version 2 of HUND.)
C     !DASH
      save
C     !DASH
      real*8 CH, CHKI, CI, CKI, DNU, HNUKT, SE, TE, X, XNC, XNE, XNU,
     $       XNUC
      integer IX, J, NO
      logical DUMP
C     !DASH
      external CHIGNON, PROD, DIVIDE, HI, BYE
C
      dimension X(*), IX(*)
C
      data DUMP /.false./
C
      call HI ('HUND')
C     !BEG
      call CHIGNON (X, IX, J, TE, XNC, DUMP, CI)
      DNU = XNUC-XNU
      call PROD    (TE, DNU, 1, HNUKT, SE)
      call DIVIDE  (CHKI, (XNE*SE), CH)
C
      write (NO,100) J,CI,CH,CKI,CHKI
  100 format(' ',I5,5X,1P2E14.6,5X,2E14.6)
C     !END
      call BYE ('HUND')
C
      return
      end
