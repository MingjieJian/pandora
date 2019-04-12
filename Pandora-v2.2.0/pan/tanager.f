      subroutine TANAGER
     $(PFO,PFE,PRTLM,N)
C
C     Rudolf Loeser, 2001 Jun 28
C---- Edits PFO, the partition function computed by the Hamburg
C     routines; returns edited values in PFE.
C     (This is version 2 of TANAGER.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, PFE, PFO, PRTLM
      integer I, IMAX, IMIN, N
C     !DASH
      external  MINMAXD, HI, BYE
      intrinsic min
C
C               PFO(N), PFE(N)
      dimension PFO(*), PFE(*)
C
      call HI ('TANAGER')
C     !BEG
      call MINMAXD (PFO,1,N,IMIN,IMAX)
      CRIT = PRTLM*PFO(IMIN)
      do 100 I = 1,N
        PFE(I) = min(PFO(I),CRIT)
  100 continue
C     !END
      call BYE ('TANAGER')
C
      return
      end
