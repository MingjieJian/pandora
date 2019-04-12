      subroutine SHAITAN
     $(XCBL,XLCR,NCR,N,TNUL)
C
C     Rudolf Loeser, 1975 Jun 11
C---- Saves TAU values in TNUL, for SHIVER.
C     !DASH
      save
C     !DASH
      real*8 TNUL, XCBL, XLCR
      integer J, KKTAUK, N, NCR
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(12),KKTAUK)
C     !DASH
      external LAMAR, MOVE1, HI, BYE
C
C               TNUL(N,NCR), XLCR(NCR), XCBL(Miklen)
      dimension TNUL(N,*),   XLCR(*),   XCBL(*)
C
      call HI ('SHAITAN')
C     !BEG
      if(NCR.gt.0) then
        do 100 J = 1,NCR
          call LAMAR (XLCR(J),0,XCBL)
          call MOVE1 (XCBL(KKTAUK),N,TNUL(1,J))
  100   continue
      end if
C     !END
      call BYE ('SHAITAN')
C
      return
      end
