      subroutine MIKE
     $(X,W,N,IS,IE,PHI,COP,GTN,LABEL,TNU,KODE,IMG)
C
C     Rudolf Loeser, 1971 Apr 07
C---- Computes monochromatic optical depth, for a particular frequency
C     in a line profile.
C     Upon return, if KODE .gt. 0, then KODE specifies the index of the
C     first nonmonotonic TNU value; KODE .eq. 0 means TNU is monotonic.
C     !DASH
      save
C     !DASH
      real*8 COP, GTN, PHI, TNU, W, X
      integer IE, IMG, IS, KODE, N
      logical lummy1, lummy2
      character LABEL*(*)
C     !DASH
      external CHARLEY, BUMBLE, HI, BYE
C
      dimension X(*), W(*)
C
C               PHI(N), COP(N), GTN(N), TNU(N), IMG(N)
      dimension PHI(*), COP(*), GTN(*), TNU(*), IMG(*)
C
      call HI ('MIKE')
C     !BEG
      call CHARLEY  (X, W, IS, IE, PHI, COP, GTN, TNU, LABEL, KODE,
     $               lummy1, lummy2, IMG)
C
      if(KODE.gt.0) then
C----   Trouble: print message
        call BUMBLE (LABEL, KODE, TNU, N, 'MIKE', 3)
      end if
C     !END
      call BYE ('MIKE')
C
      return
      end
