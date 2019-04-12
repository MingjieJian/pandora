      subroutine BUSTARD
     $(XLM,KUPT,KOPAC,NOPAC,N,CO,T1,TR,F,CAPPAR,CAPPA,SIGMA,OPAC,
     $ SCAT,SIGSTR)
C
C     Rudolf Loeser, 1986 Jul 12
C---- Collects the various opacity constituents.
C
C     When KUPT > 0, KUPT is the number of the contributor whose
C     components have been reserved.
C     !DASH
      save
C     !DASH
      real*8 CAPPA, CAPPAR, CO, F, OPAC, SCAT, SIGMA, SIGSTR, T1, TR,
     $       XLM
      integer I, KOPAC, KUPT, N, NOPAC
      logical UPT
C     !DASH
      external LEFT, PERIDOT, CUSP, DIVIDE, HI, BYE
C
C               CO(Nopac,N), OPAC(N), SCAT(N), KOPAC(Nopac), CAPPAR(N),
      dimension CO(NOPAC,*), OPAC(*), SCAT(*), KOPAC(*),     CAPPAR(*),
C
C               SIGMA(N), SIGSTR(N), CAPPA(N), T1(N), TR(N)
     $          SIGMA(*), SIGSTR(*), CAPPA(*), T1(*), TR(*)
C     !EJECT
C
      call HI ('BUSTARD')
C     !BEG
      UPT = KUPT.gt.0
C---- Loop over all depths
      do 100 I = 1,N
C----   Get scattering opacity (partial)
        call PERIDOT (KOPAC, CO(1,I), SIGSTR(I))
C----   Get pure continuum opacity without reserved opacity
        call CUSP    (KOPAC, CO(1,I), CAPPAR(I))
C----   Get total pure continuum opacity
        CAPPA(I) = CAPPAR(I)
        if(UPT) then
          CO(KUPT,I) = T1(I)+TR(I)
          CAPPA(I)   = CAPPA(I)+CO(KUPT,I)
        end if
C----   Get total opacity
        OPAC(I) = F*(SIGSTR(I)+CAPPA(I))
C----   Get scattering opacity
        SIGMA(I) = F*SIGSTR(I)
C----   Get scattering ratio
        call DIVIDE  (SIGSTR(I), CAPPA(I), SCAT(I))
  100 continue
C     !END
      call BYE ('BUSTARD')
C
      return
      end
