      subroutine TYKE
     $(X,W,KOLEV,K,N,FTK,TNU,IMG)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Computes optical depth, for TEUFEL.
C     !DASH
      save
C     !DASH
      real*8 FTK, TNU, W, X
      integer IMG, JUNK, K, KOLEV, N
      logical lummy1, lummy2
      character LABEL*100
C     !DASH
      external DISMAL, BUMBLE, HI, BYE
C
      dimension X(*), W(*)
C
C               FTK(N), TNU(N), IMG(N)
      dimension FTK(*), TNU(*), IMG(*)
C
C
      call HI ('TYKE')
C     !BEG
      write (LABEL,100) KOLEV,K
  100 format('Level',I3,' to Continuum optical depth, for',I3,
     $       '. frequency',46X)
C
      call DISMAL   (X, W, 1, N, FTK, TNU, LABEL, JUNK, lummy1, lummy2,
     $               IMG)
C
      if(JUNK.gt.0) then
C----   Trouble: print message and stop
        call BUMBLE (LABEL, JUNK, TNU, N, 'TYKE', 1)
      end if
C     !END
      call BYE ('TYKE')
C
      return
      end
