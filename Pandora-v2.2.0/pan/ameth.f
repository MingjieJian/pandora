      subroutine AMETH
     $(NLY,XLM,TE,XNE,V,H1,XLMXX,XLMDR,LLY,ITAU,EN,WN,AN,SA,SF,P,
     $ OPAC,SCAT,DMPI)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Computes higher H Ly lines background opacities and scatterings.
C     (This is version 2 of AMETH.)
C     !DASH
      save
C     !DASH
      real*8 A, AN, AN1, AN1S, CALL, CLN, DR, EMM, EN, FACT, FALL, H1,
     $       OPAC, OPACN, OPACX, P, PHI, RAYS, RES, ROOT, SA, SCAT,
     $       SCATN, SF, SK, STKFN, TE, TERM, V, WLIN, WN, X, XLIM, XLM,
     $       XLMDR, XLMXX, XLR, XLTR, XNE, XNU, Z, ZERO
      integer I, IFALL, IND, ITAU, K, LLY, N, NLY
      logical DMPI
C     !COM
C---- HILYLI      as of 2005 Dec 22
      integer     LYLINO
      common      /HILYLI/ LYLINO
C     Index of upper level, if this is an H Lyman line wavelength
C     (see subroutine ISTUR).
C     .
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(187),IFALL)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !EJECT
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external JELLA, KEITH, GERUND, MEDICK, CORUND, DIMECK, MICKED,
     $         HALT, NARGO, HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY), EN(NLL), AN(NLL,N), SA(NLL,N),
      dimension XLMXX(*),   XLMDR(*),   EN(*),   AN(NLL,*), SA(NLL,*),
C
C               P(NLY), WN(NLL), SF(NLL)
     $          P(*),   WN(*),   SF(*)
C
      call HI ('AMETH')
C     !BEG
      P(1) = ZERO
      P(2) = ZERO
      OPAC = ZERO
      SCAT = ZERO
C
      if(DMPI) then
        call JELLA (XLM, ITAU, TE, V, H1, NLY, LYLINO)
      end if
      call NARGO   (ITAU, K)
C     !EJECT
      do 101 I = 3,NLY
        IND  = (NLL+1)-I
        WLIN = WN(IND)
        AN1  = AN(IND,K)
        AN1S = SA(IND,K)
        SK   = SF(IND)
        N    = EN(IND)
        if(N.ne.I) then
          write (MSSLIN(1),100) N,I
  100     format(' ','N =',I12,', which is not equal to I =',I12)
          call HALT       ('AMETH', 1)
        end if
C
        if(N.eq.LYLINO) then
          P(N) = ZERO
        else
C
          call KEITH      (N, XLM, TE, XNE, V, XLMXX, WLIN, XNU, ROOT,
     $                     CLN, AN1, AN1S, SK, STKFN, XLIM, X)
          if(X.le.XLIM) then
            call GERUND   (N, H1, CLN, ROOT, AN1, AN1S, STKFN, X, A,
     $                     PHI, OPACX, SCATN)
            if(DMPI) then
              call MEDICK (N, AN1, AN1S, STKFN, X, A, PHI, OPACX, SCATN)
            end if
          else
            call CORUND   (N, XLM, TE, H1, XLMXX, XLMDR, LLY, WLIN, AN1,
     $                     AN1S, STKFN, CLN, X, IFALL, XLR, XLTR, EMM,
     $                     RES, CALL, FALL, FACT, Z, DR, TERM, RAYS,
     $                     OPACX, SCATN)
            if(DMPI) then
              call DIMECK (N, AN1, AN1S, STKFN, X, DR, TERM, RAYS,
     $                     OPACX, SCATN)
            end if
          end if
C
          if(N.le.15) then
            OPACN = FLNRML(N)*OPACX
          else
            OPACN = OPACX
          end if
C
          P(N) = OPACN
          OPAC = OPAC+OPACN
          SCAT = SCAT+SCATN
C
        end if
  101 continue
      if(DMPI) then
        call MICKED       (OPAC, SCAT)
      end if
C     !END
      call BYE ('AMETH')
C
      return
      end
