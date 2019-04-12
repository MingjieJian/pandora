      subroutine LILITH
     $(X,W,IW,XLM,INCDNT,JNUMTH,N,XJNU,OPAC,SOURCE,SCAT,YDAMP,TAU,ITS,
     $ FD,BHS,CNDT,IMG,IIFLAG,CNXP,SIGMA,BHSNUM,EXT)
C
C     Rudolf Loeser, 1981 Jul 21.
C---- Computes continuum source function, SOURCE,
C     and intensity, XJNU, at wavelength = XLM.
C     (This is version 4 of LILITH.)
C
C     (See also "VENUS".)
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNUM, CNDT, CNXP, EXT, FD, OPAC, SCAT, SIGMA,
     $       SOURCE, TAU, W, X, XJNU, XLM, YDAMP
      integer II, IIFLAG, IMG, ITS, IW, JNUMTH, KODE, LAG, N, NR
      logical DUMP, INCDNT
C     !DASH
      external LILTO, LILTA, ARNICA, ALEPH, LILTI, OCTOPUS, REED, BORA,
     $         ZERO1, BITTER, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               SOURCE(N), XJNU(N), CNDT(N), SIGMA(N), OPAC(N), TAU(N),
      dimension SOURCE(*), XJNU(*), CNDT(*), SIGMA(*), OPAC(*), TAU(*),
C
C               EXT(N), IMG(N), BHS(N), SCAT(N), CNXP(N), BHSNUM(*),
     $          EXT(*), IMG(*), BHS(*), SCAT(*), CNXP(*), BHSNUM(*),
C
C               FD(N)
     $          FD(*)
C     !EJECT
C
      call HI ('LILITH')
C     !BEG
C---- Set up dump switch ( ? print header)
      call BITTER    (XLM, DUMP)
      if(DUMP) then
        call LILTA   (XLM, 'LILITH')
      end if
C---- Initialize
      ITS    = -1
      IIFLAG = 1
      call ZERO1     (SOURCE, N)
      call ZERO1     (XJNU  , N)
      call ZERO1     (FD    , N)
C
C---- Compute Optical Depth, and reduced table index
      call ALEPH     (X, W, XLM, OPAC, TAU, IMG)
      call BORA      (TAU, N, XLM, KODE, II, NR)
      if(DUMP) then
        call LILTI   (II, N, NR, YDAMP)
      end if
C
      if(KODE.eq.1) then
C----   Compute Incident Radiation term
        call OCTOPUS (XLM, X, INCDNT, TAU, EXT, CNDT, CNXP)
C----   Compute Intensity and Continuum Source Function
        call REED    (X, W, IW, XLM, DUMP, JNUMTH, TAU, BHS, SCAT,
     $                CNDT, CNXP, OPAC, SIGMA, BHSNUM, II, NR, N,
     $                YDAMP, XJNU, SOURCE, ITS, LAG, IMG)
C----   Compute Flux Derivative
        call ARNICA  (N, SOURCE, OPAC, XJNU, FD)
C----   Compute exit flag
        IIFLAG = 1000*LAG+II
      end if
C
      if(DUMP) then
C       Print flags and trailer
        call LILTO   (IIFLAG, ITS, 'LILITH')
      end if
C     !END
      call BYE ('LILITH')
C
      return
      end
