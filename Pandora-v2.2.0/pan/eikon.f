      subroutine EIKON
     $(SAVE,NMAX,NS,X,U,F,G)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Saves data if possible, for ENTE.
C     (This is version 2 of EIKON.)
C     !DASH
      save
C     !DASH
      real*4 F, G, SAVE, U, X
      integer NMAX, NS
C     !DASH
      external HI, BYE
C
C               SAVE(NMAX,4)
      dimension SAVE(NMAX,*)
C
      call HI ('EIKON')
C     !BEG
      NS = NS+1
C
      if(NS.le.NMAX) then
        SAVE(NS,1) = X
        SAVE(NS,2) = U
        SAVE(NS,3) = F
        SAVE(NS,4) = G
      end if
C     !END
      call BYE ('EIKON')
C
      return
      end
