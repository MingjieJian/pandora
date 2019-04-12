      subroutine CHARLEY
     $(X,W,IBEG,IEND,PHI,COP,GTN,TNU,LABEL,KODE,EDINT,EDTAU,IMG)
C
C     Rudolf Loeser, 1971 Jan 06
C---- Computes and checks an optical depth scale. Upon return,
C     KODE .eq. 0 if the TNU values are nondecreasing, .gt. 0 if not.
C     !DASH
      save
C     !DASH
      real*8 COP, GTN, PHI, TNU, W, X
      integer IBEG, IEND, IMG, IN, IOPAC, IS, IXKPL, KODE, MOX
      logical EDINT, EDTAU
      character LABEL*(*)
C     !DASH
      external  MERMAID, ELSI, DISMAL, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               PHI(N), COP(N), GTN(N), TNU(N), IMG(N)
      dimension PHI(*), COP(*), GTN(*), TNU(*), IMG(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IOPAC ),(IN( 2),IXKPL )
C
      call HI ('CHARLEY')
C     !BEG
C     (Get, and allocate, W allotment)
      call MERMAID (IN, IS, MOX, 'CHARLEY')
C
      call ELSI    (IBEG, IEND, PHI, COP, GTN, W(IXKPL), W(IOPAC))
      call DISMAL  (X, W, IBEG, IEND, W(IOPAC), TNU, LABEL, KODE,
     $              EDINT, EDTAU, IMG)
C
C     (Give back W allotment)
      call WGIVE   (W, 'CHARLEY')
C     !END
      call BYE ('CHARLEY')
C
      return
      end
