      subroutine VENUS
     $(X,W,IW,XCBL,XLM,N,LG,MRR,XJNU,OPAC,SOURCE,SCAT,YDAMP,TAU,ITS,FD,
     $ BHS,CNDT,IMG,IIFLAG,CNXP,SIGMA,BHSNUM,EXT,XLTIT,MOVING,SPHERE,
     $ INCDNT,JNUMTH,ILFLX,MPROM,XLB1,XLB2)
C
C     Rudolf Loeser, 1983 Jan 19.
C---- Computes continuum source function, SOURCE,
C     and intensity, XJNU, at wavelength = XLM.
C     (This is version 2 of VENUS.)
C
C     (Special version of "LILITH", for P.R.D. Jnu calculation.)
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNUM, CNDT, CNXP, EXT, FD, OPAC, SCAT, SIGMA,
     $       SOURCE, TAU, W, X, XCBL, XJNU, XLB1, XLB2, XLM, XLTIT,
     $       YDAMP
      integer IIFLAG, ILFLX, IMG, INCDNT, IREF, ITS, IW, JNUMTH, LAG,
     $        LG, MPROM, MRR, N
      logical DUMP, MOVING, SPHERE
C     !DASH
      external BITTER, HANIBAL, ARNICA, ALEPH, LILTI, OCTOPUS, LINCOLN,
     $         LILTA, ZERO1, LILTO, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen), BHSNUM(N), SIGMA(N), XLB1(Li1len), FD(N),
      dimension XCBL(*),      BHSNUM(*), SIGMA(*), XLB1(*),      FD(*),
C
C               SCAT(N), CNDT(N), CNXP(N), XJNU(N), OPAC(N), SOURCE(N),
     $          SCAT(*), CNDT(*), CNXP(*), XJNU(*), OPAC(*), SOURCE(*),
C
C               TAU(N), EXT(N), IMG(N), BHS(N), XLB2(Li2len)
     $          TAU(*), EXT(*), IMG(*), BHS(*), XLB2(*)
C
      data IREF /0/
C
      call HI ('VENUS')
C     !BEG
C---- Set up dump switch ( ? print header)
      call BITTER  (XLM, DUMP)
      if(DUMP) then
        call LILTA (XLM, 'VENUS')
        call LILTI (IREF, N, N, YDAMP)
      end if
C---- Initialize
      ITS    = -1
      IIFLAG =  1
      call ZERO1   (SOURCE, N)
      call ZERO1   (XJNU,   N)
      call ZERO1   (FD,     N)
C     !EJECT
C---- Compute optical depth
      call ALEPH     (X, W, XLM, OPAC, TAU, IMG)
C---- Compute incident radiation term
      call OCTOPUS   (XLM, X, INCDNT, TAU, EXT, CNDT, CNXP)
C
C---- Compute Intensity and Continuum Source function
      if(.not.MOVING) then
C
C----   Angle-independent case
        call LINCOLN (X, W, IW, XLB1, XLB2, XLM, DUMP, TAU, BHS, SCAT,
     $                CNDT, CNXP, OPAC, SIGMA, BHSNUM, N, YDAMP,
     $                MOVING, ILFLX, JNUMTH, XJNU, SOURCE, ITS, LAG,
     $                XLTIT, IMG)
      else
C
C----   Angle-dependent case (through line opacity)
        call HANIBAL (X, W, IW, XLB1, XLB2, XLM, DUMP, XCBL, YDAMP,
     $                ILFLX, MPROM, MOVING, SPHERE, N, LG, MRR, XJNU,
     $                SOURCE, ITS, LAG, IMG)
      end if
C
C---- Compute flux derivative
      call ARNICA    (N, SOURCE, OPAC, XJNU, FD)
C
C---- Compute exit flag ( ? and print it)
      IIFLAG = 1000*LAG
      if(DUMP) then
        call LILTO   (IIFLAG, ITS, 'VENUS')
      end if
C     !END
      call BYE ('VENUS')
C
      return
      end
