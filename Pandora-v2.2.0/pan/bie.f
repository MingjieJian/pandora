      subroutine BIE
     $(XLM,TE,XNE,V,H1,XLMXX,XLMDR,LLY,ITAU,EN,WN,AN,SA,SF,OPAC,SCAT,
     $ DMPI)
C
C     Rudolf Loeser, 1995 May 11
C           revised, 2002 Sep 18
C
C---- Computes H Lyman alpha background opacity and scattering.
C     (This is version 2 of BIE.)
C     !DASH
      save
C     !DASH
      real*8 A, A21, A21S, AN, CALL, CL2, DR, EMM, EN, FACT, FALL, H1,
     $       OPAC, PHI, RAYS, RES, ROOT, SA, SCAT, SF, SK, STKFN, TE,
     $       TERM, V, WLIN, WN, X, XLIM, XLM, XLMDR, XLMXX, XLR, XLTR,
     $       XNE, XNU2, Z
      integer IFALL, INR, ITAU, K, LLY, N, NR
      logical DMPI
C     !COM
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
      external KEITH, GERUND, CORUND, OUBLIER, SINJA, ZINJA, NARGO,
     $         HALT, HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY), AN(NLL,N), SA(NLL,N), WN(NLL),
      dimension XLMXX(*),   XLMDR(*),   AN(NLL,*), SA(NLL,*), WN(*),
C
C               SF(NLL), EN(NLL)
     $          SF(*),   EN(*)
C
      data NR /2/
C     !EJECT
C
      call HI ('BIE')
C     !BEG
C---- Get X, distance from line center in Doppler widths (? print)
      call NARGO     (ITAU, K)
      INR = (NLL+1)-NR
      N   = EN(INR)
      if(N.ne.NR) then
        write (MSSLIN(1),100) N,NR
  100   format(' ','N =',I12,', which is not equal to NR =',I12)
        call HALT    ('BIE', 1)
      end if
      WLIN = WN(INR)
      A21  = AN(INR,K)
      A21S = SA(INR,K)
      SK   = SF(INR)
      call KEITH     (N, XLM, TE, XNE, V, XLMXX, WLIN, XNU2, ROOT, CL2,
     $                A21, A21S, SK, STKFN, XLIM, X)
      if(DMPI) then
        call OUBLIER (XLM, ITAU, TE, V, H1, XNU2, ROOT, CL2, A21, A21S,
     $                XLIM, X)
      end if
C
      if(X.le.XLIM) then
C----   Line core
        call GERUND  (N, H1, CL2, ROOT, A21, A21S, STKFN, X, A, PHI,
     $                OPAC, SCAT)
        if(DMPI) then
          call SINJA (STKFN, A, PHI, OPAC, SCAT)
        end if
      else
C----   Line wings
        call CORUND  (N, XLM, TE, H1, XLMXX, XLMDR, LLY, WLIN, A21,
     $                A21S, STKFN, CL2, X, IFALL, XLR, XLTR, EMM, RES,
     $                CALL, FALL, FACT, Z, DR, TERM, RAYS, OPAC, SCAT)
        if(DMPI) then
          call ZINJA (STKFN, XLR, XLTR, EMM, RES, IFALL, CALL, FALL,
     $                FACT, Z, DR, TERM, RAYS, OPAC, SCAT)
        end if
      end if
C     !END
      call BYE ('BIE')
C
      return
      end
