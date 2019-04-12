      subroutine ROWAN
     $(INDX,XLM,N,NOPAC,TE,XNE,XNC,V,HN,BDH,CO,PL,UL,EU,VL,XL,EN,
     $ WN,DN,AN,SA,SF,TN,PN,ST,AA,XX)
C
C     Rudolf Loeser, 2003 Jan 06
C---- Computes opacity contributions from the highest H Ly lines.
C     (This is version 3 of ROWAN.)
C     !DASH
      save
C     !DASH
      real*8 AA, AN, BDH, CO, CP1, DN, EN, EU, FAC, G, G1, HN, PE, PL,
     $       PN, PW, SA, SF, SLM, ST, TE, TN, UL, V, VL, WLM, WN, XKA,
     $       XKH, XL, XLM, XNC, XNE, XX, ZERO, ZLM
      integer I, INDX, K, LYODS, N, NOPAC
      logical DMPI, DUMP, YES
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
      equivalence (KZQ( 89),LYODS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     !DASH
C     !EJECT
      external  TULLEM, DIKKER, NAGOR, GORNA, GUNNAR, MULLET, ZEROD,
     $          MINNA, NARGO, BROWN, MALTA, YALTA, HI, BYE
      intrinsic max
C
C               HN(N,Limdat(1)), BDH(N,Limdat(1)), CO(Nopac,N), XNE(N),
      dimension HN(N,*),         BDH(N,*),         CO(NOPAC,*), XNE(*),
C
C               AN(NLL,N), SA(NLL,N), WN(NLL), EN(NLL), DN(NLL), TE(N),
     $          AN(NLL,*), SA(NLL,*), WN(*),   EN(*),   DN(*),   TE(*),
C
C               V(N), PL(N), UL(N), PN(NLL), ST(NLL), AA(NLL), XX(NLL),
     $          V(*), PL(*), UL(*), PN(*),   ST(*),   AA(*),   XX(*),
C
C               VL(N), XL(N), SF(NLL), TN(NLL), XNC(N), EU(N)
     $          VL(*), XL(*), SF(*),   TN(*),   XNC(*), EU(*)
C
      data ZLM,SLM,CP1  /9.119D2, -1.D0, 0.D0/
C
      data PW,FAC /6.6666666666666667D-1, 1.D12/
C     !EJECT
C
      call HI ('ROWAN')
C     !BEG
      if(CP1.eq.ZERO) then
C----   Set up CP1
        call BROWN    (1, 1, CP1)
      end if
C
      call DIKKER     (XLM, YES, DUMP)
C
      if(YES) then
C----   Don't compute all the way to the edge
        WLM = max(XLM,ZLM)
C
C----   Set up intermediates ( ? print)
        if(WLM.ne.SLM) then
          SLM = WLM
          call GUNNAR (1, SLM, G1)
        end if
        call GORNA    (WN, AN, N, TE, V, HN(1,1), BDH(1,1), PL, UL,
     $                 EU, VL, XL)
        if(DUMP) then
          call NAGOR  (XLM, WLM, CP1, G1, N, TE, XNE, XNC, V, HN(1,1),
     $                 BDH(1,1), PL, UL, VL, XL)
        end if
C
        call MALTA    (XLM, DUMP, 'ROWAN')
        do 100 I = 1,N
          call MINNA  (DUMP, I, LYODS, DMPI)
C----     Compute actual opacity ( ? print)
          PE = (XNE(I)/FAC)**PW
          call NARGO  (I, K)
          call MULLET (WLM, EN, WN, DN, AN(1,K), SA(1,K), SF, PE, XX,
     $                 AA, PN, ST, TN, NLL, XL(I), HN(I,1), I, G, DMPI)
          XKH = PL(I)*G
C----     Compute opacity at edge, for normalization
          XKA = (CP1*G1)*VL(I)*HN(I,1)
C----     Compute normalized value ( ? print)
          call TULLEM (I, XLM, XKH, XKA, CO(INDX,I), DMPI)
  100   continue
        call YALTA    (DUMP, 'ROWAN')
C
      else
        call ZEROD    (CO(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('ROWAN')
C
      return
      end
