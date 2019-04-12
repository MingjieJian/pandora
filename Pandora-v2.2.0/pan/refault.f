      subroutine REFAULT
     $(MODE,M,KS,LF,MRR,NTE,MHM,JM,LDU,NDT,MQT,NDR,LG,NCL,NVH,NCQ,KBX,
     $ NFH,INK,NLN,NGM,FRR,TS,XIS,EMUF,TER,XLHM,AHM,XLMM,XMLC,XLMDUST,
     $ XLDT,ADT,ALBDT,CQTAIL,AL,XDR,DDR,XMU,XCOL,VNH,HNDV,CQT,CQA,BXI,
     $ HNDF,FNH,XINK,FINK,WSLN,ZGM,DGMZ)
C
C     Rudolf Loeser, 1968 Sep 30
C     Revised RL/SGK Jul 14 2014 
C---- Defaults for input tables and their lengths.
C     !DASH
      save
C     !DASH
      real*8 ADT, ADTX, AHM, AHMX, AL, ALBDT, ALBDTX, BXI, BXIX, CQA,
     $       CQAX, CQT, CQTAIL, CQTAILX, CQTX, DDR, DDRX, DGMZ, DGMZX,
     $       EMUF, EMUFX, FINK, FINKX, FNH, FNHX, FRR, FRRX, HNDF,
     $       HNDFX, HNDV, HNDVX, ONE, TER, TERX, TS, TSX, VNH, VNHX,
     $       WSLN, WSLNX, XCOL, XCOLX, XDR, XDRX, XINK, XINKX, XIS, XIX,
     $       XLDT, XLDTX, XLHM, XLHMX, XLMDSTX, XLMDUST, XLMM, XLMMX,
     $       XMLC, XMLCX, XMU, XMUX, ZGM, ZGMX
      integer INK, INKX, JM, JMX, KBX, KBXX, KS, KX, LDU, LDUX, LF, LFX,
     $        LG, LGX, M, MHM, MHMX, MODE, MQT, MQTX, MRR, MRRX, MX,
     $        NCL, NCLX, NCQ, NCQX, NDR, NDRX, NDT, NDTX, NFH, NFHX,
     $        NGM, NGMX, NLN, NLNX, NTE, NTEX, NVH, NVHX
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- OPTIONS     as of Jul 14 2014 (RL/SGK)
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=346)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external MOVED, MOVEI, HALT, HI, BYE
C
C               TS(M), AHM(MHM), XIS(KS), TER(NTE), XLHM(MHM), XMU(LG),
      dimension TS(*), AHM(*),   XIS(*),  TER(*),   XLHM(*),   XMU(*),
C
C               ALBDT(NDT), XLMM(JM), XMLC(JM), XLMDUST(LDU), EMUF(LF),
     $          ALBDT(*),   XLMM(*),  XMLC(*),  XLMDUST(*),   EMUF(*),
C
C               VNH(NVH), CQTAIL(MQT), XLDT(NDT), XCOL(NCL), HNDV(NVH),
     $          VNH(*),   CQTAIL(*),   XLDT(*),   XCOL(*),   HNDV(*),
C
C               HNDF(NFH), WSLN(NLN), XINK(INK), FINK(INK), DGMZ(NGM),
     $          HNDF(*),   WSLN(*),   XINK(*),   FINK(*),   DGMZ(*),
C
C               CQA(NCQ), FRR(MRR), BXI(KBX), FNH(NFH), CQT(NCQ),
     $          CQA(*),   FRR(*),   BXI(*),   FNH(*),   CQT(*),
C
C               XDR(NDR), AL(NL), ZGM(NGM), DDR(NDR), ADT(NDT)
     $          XDR(*),   AL(*),  ZGM(*),   DDR(*),   ADT(*)
C
      dimension TSX(33), CQTX(5), XIX(24), EMUFX(2), FRRX(6), VNHX(38),
     $          TERX(1), CQAX(5), AHMX(34), XMUX(8), XLHMX(34),
     $          CQTAILX(3), XLDTX(29), ADTX(29), XLMDSTX(1), XCOLX(5),
     $          XMLCX(1), XLMMX(1), ALBDTX(29), HNDVX(38), XDRX(7),
     $          DDRX(7), BXIX(25), HNDFX(8), FNHX(8), XINKX(6),
     $          FINKX(6), WSLNX(27), ZGMX(23), DGMZX(23)
C
      data MX /33/
      data TSX /
     $ 0.D0, 1.D-4, 2.D-4, 3.D-4, 6.D-4, 1.D-3, 2.D-3, 3.D-3, 6.D-3,
     $ 1.D-2, 2.D-2, 3.D-2, 6.D-2, 1.D-1, 2.D-1, 3.D-1, 6.D-1,
     $ 1.D0, 2.D0, 3.D0, 5.D0, 7.D0, 1.D+1, 1.5D+1, 2.D+1, 3.D+1,
     $ 5.D+1, 7.5D+1, 1.D+2, 1.5D+2, 2.D+2, 3.D+2, 5.D+2/
C
      data KX /24/
      data XIX /
     $ 0.D0, 1.D-1, 2.D-1, 3.D-1, 4.D-1, 5.D-1, 6.D-1, 7.D-1, 8.D-1,
     $ 9.D-1, 1.1D0, 1.3D0, 1.5D0, 1.8D0, 2.1D0, 2.4D0, 2.8D0, 3.4D0,
     $ 3.9D0, 4.5D0, 6.D0, 8.D0, 1.5D+1, 5.D+1/
C
      data LFX /2/
      data EMUFX /1.D0, 3.D-1/
C
C     (XLHM, etc., must be rearranged into ascending order before use.)
      data MHMX /34/
      data XLHMX /
     $ 16.300D3, 16.200D3, 16.000D3, 15.500D3, 15.000D3, 14.500D3,
     $ 14.000D3, 13.500D3, 13.000D3, 12.500D3, 12.000D3, 11.500D3,
     $ 11.000D3, 10.500D3, 10.000D3, 9.5000D3, 9.0000D3, 8.5000D3,
     $ 8.0000D3, 7.5000D3, 7.0000D3, 6.5000D3, 6.0000D3, 5.5000D3,
     $ 5.0000D3, 4.5000D3, 4.0000D3, 3.5000D3, 3.0000D3, 2.5000D3,
     $ 2.0000D3, 1.7500D3, 1.5000D3, 1.2500D3/
      data AHMX  /
     $ .01989D0, .04974D0, .13020D0, .40520D0, .74070D0, 1.1070D0,
     $ 1.4850D0, 1.8620D0, 2.2260D0, 2.5710D0, 2.8870D0, 3.1720D0,
     $ 3.4190D0, 3.6250D0, 3.7890D0, 3.9060D0, 3.9770D0, 4.0010D0,
     $ 3.9770D0, 3.9070D0, 3.7910D0, 3.6320D0, 3.4320D0, 3.1940D0,
     $ 2.9230D0, 2.6240D0, 2.3020D0, 1.9650D0, 1.6190D0, 1.2750D0,
     $ .94530D0, .79180D0, .65120D0, .54310D0/
C
      data MRRX /6/
      data FRRX /0.D0, 5.D-1, 8.D-1, 9.D-1, 9.5D-1, 1.D0/
C
      data NTEX /1/
      data TERX /4.D+3/
C
      data JMX /1/
      data XLMMX /1.682D+3/
      data XMLCX /1.D0/
C
      data LDUX /1/
      data XLMDSTX /5.D+3/
C
      data NDTX /29/
      data XLDTX /
     $ 9.1D+2, 9.52D+2, 1.D+3, 1.05D+3, 1.11D+3, 1.18D+3, 1.25D+3,
     $ 1.33D+3, 1.43D+3, 1.54D+3, 1.67D+3, 1.82D+3, 2.D+3, 2.08D+3,
     $ 2.17D+3, 2.27D+3, 2.38D+3, 2.5D+3, 3.3D+3, 5.D+3, 1.D+4,
     $ 3.D+4, 1.D+5, 3.D+5, 1.D+6, 3.D+6, 1.D+7, 3.D+7, 1.D+8/
      data ADTX /
     $ 5.1D0, 4.7D0, 4.3D0, 3.8D0, 3.4D0, 3.D0, 2.65D0, 2.4D0,
     $ 2.2D0, 2.1D0, 2.1D0, 2.D0, 2.1D0, 2.3D0, 2.3D0, 2.2D0,
     $ 1.9D0, 1.8D0, 1.3D0, 9.3D-1, 3.4D-1, 8*3.D-1/
      data ALBDTX /29*9.D-1/
C
      data MQTX /3/
      data CQTAILX /5.D-1, 1.D-1, 1.D-2/
C
      data NDRX /7/
      data XDRX /5.5D0, 6.D0, 7.D0, 8.D0, 1.D+1, 1.2D+1, 1.5D+1/
      data DDRX /1.D0, 9.D-1, 6.5D-1, 4.D-1, 1.D-1, 5.D-2, 0.D0/
C
      data LGX /8/
      data XMUX /
     $ 1.D0, 8.D-1, 6.D-1, 5.D-1, 4.D-1, 3.D-1, 2.D-1, 1.D-1/
C
      data NVHX /38/
      data HNDVX /
     $ 1.00D09, 2.00D09, 5.00D09, 1.00D10, 2.00D10, 5.00D10,
     $ 7.59D10, 9.55D10, 1.61D11, 3.17D11, 7.73D11, 2.71D12,
     $ 9.32D12, 2.04D13, 6.69D13, 9.82D13, 2.25D14, 3.55D14,
     $ 6.01D14, 9.87D14, 1.64D15, 2.09D15, 3.37D15, 4.22D15,
     $ 6.58D15, 1.02D16, 2.35D16, 4.24D16, 6.05D16, 8.33D16,
     $ 1.03D17, 1.15D17, 1.22D17, 1.27D17, 1.30D17, 1.32D17,
     $ 1.34D17, 1.35D17 /
      data VNHX /
     $ 15.18, 13.68, 11.92, 10.75,  9.68,  8.44,  7.81,  7.52,
     $  6.95,  6.28,  5.52,  4.60,  3.59,  2.98,  2.20,  2.00,
     $  1.54,  1.38,  1.18,  1.00,  0.86,  0.80,  0.68,  0.65,
     $  0.55,  0.52,  0.63,  0.90,  1.10,  1.30,  1.46,  1.56,
     $  1.64,  1.71,  1.76,  1.80,  1.82,  1.83   /
C
      data NFHX /8/
      data HNDFX /
     $ 1.D11, 3.D11, 1.D12, 3.D12, 1.D13, 3.D13, 1.D14, 3.D14/
      data FNHX /
     $ 1.D1,  9.D0,  7.D0,  5.D0,  3.D0,  2.D0,  1.D0,  0.D0/
C
      data NCLX /5/
      data XCOLX /
     $ 0.D0, 5.D-1, 1.D0, 1.5D0, 2.D0/
C
      data NCQX /5/
      data CQTX /
     $ 4.D3,  5.D3,  6.D3,  7.D3,  8.D3/
      data CQAX /
     $ 1.D-4, 1.D-3, 1.D-2, 1.D-1, 1.D+0/
C
      data KBXX /25/
      data BXIX /
     $ 0.0D+0, 3.0D-2, 6.0D-2, 1.0D-1, 1.5D-1, 2.2D-1, 3.0D-1, 4.0D-1,
     $ 6.0D-1, 9.0D-1, 1.2D+0, 1.5D+0, 2.0D+0, 3.0D+0, 5.0D+0, 1.0D+1,
     $ 2.0D+1, 4.0D+1, 7.0D+1, 1.1D+2, 2.0D+2, 5.0D+2, 1.0D+3, 2.0D+3,
     $ 5.0D+3/
C
      data INKX /6/
      data XINKX /
     $ 5.948D0, 5.949D0, 7.48D0, 7.51D0, 1.316D1, 1.317D1/
      data FINKX /
     $ 0.D0, 3.D-12, 3.D-12, 1.5D-12, 1.5D-12, 0.D0/
C
      data NLNX /27/
      data WSLNX /
     $ 0.D0, 5.0D-2, 1.0D-1, 1.5D-1, 2.0D-1, 2.5D-1, 3.0D-1, 3.5D-1,
     $ 4.0D-1, 4.5D-1, 5.0D-1, 6.0D-1, 7.0D-1, 8.0D-1, 1.0D0, 1.2D0,
     $ 1.5D0, 2.0D0, 2.5D0, 3.0D0, 4.0D0, 5.0D0, 6.0D0, 8.0D0, 1.0D1,
     $ 1.5D1, 2.0D1/
C
      data NGMX /23/
      data ZGMX /
     $ -2.0D3,  -1.9D3,  -1.8D3,  -1.7D3,  -1.6D3,  -1.5D3,  -1.4D3,
     $ -1.3D3,  -1.2D3,  -1.1D3,  -1.0D3,  -9.0D2,  -8.0D2,  -7.0D2,
     $ -6.0D2,  -5.0D2,  -4.0D2,  -3.0D2,  -2.0D2,  -1.0D2,  -5.0D1,
     $  0.0D0,   5.0D1/
      data DGMZX /
     $ 0.640D0, 0.645D0, 0.660D0, 0.680D0, 0.710D0, 0.740D0, 0.770D0,
     $ 0.810D0, 0.860D0, 0.890D0, 0.920D0, 0.950D0, 0.970D0, 0.985D0,
     $ 0.990D0, 0.995D0, 0.995D0, 0.995D0, 0.990D0, 0.980D0, 0.970D0,
     $ 0.965D0, 0.960D0/
C     !EJECT
C
      call HI ('REFAULT')
C     !BEG
      if(MODE.eq.1) then
C
        M   = MX
        KS  = KX
        LF  = LFX
        MRR = MRRX
        NTE = NTEX
        MHM = MHMX
        JM  = JMX
        LDU = LDUX
        NDT = NDTX
        MQT = MQTX
        NDR = NDRX
        LG  = LGX
        NCL = NCLX
        NVH = NVHX
        NCQ = NCQX
        KBX = KBXX
        NFH = NFHX
        INK = INKX
        NLN = NLNX
        NGM = NGMX
C
        call MOVEI (IQD, 1, NOOPT, IQQ, 1, NOOPT)
C     !EJECT
      else if(MODE.eq.2) then
C
        call MOVED (TSX    , 1, MX   , TS     , 1, M    )
        call MOVED (XIX    , 1, KX   , XIS    , 1, KS   )
        call MOVED (TERX   , 1, NTEX , TER    , 1, NTE  )
        call MOVED (XLHMX  , 1, MHMX , XLHM   , 1, MHM  )
        call MOVED (AHMX   , 1, MHMX , AHM    , 1, MHM  )
        call MOVED (XLMMX  , 1, JMX  , XLMM   , 1, JM   )
        call MOVED (XMLCX  , 1, JMX  , XMLC   , 1, JM   )
        call MOVED (XLMDSTX, 1, LDUX , XLMDUST, 1, LDU  )
        call MOVED (XLDTX  , 1, NDTX , XLDT   , 1, NDT  )
        call MOVED (ADTX   , 1, NDTX , ADT    , 1, NDT  )
        call MOVED (ALBDTX , 1, NDTX , ALBDT  , 1, NDT  )
        call MOVED (CQTAILX, 1, MQTX , CQTAIL , 1, MQT  )
        call MOVED (XDRX   , 1, NDRX , XDR    , 1, NDR  )
        call MOVED (DDRX   , 1, NDRX , DDR    , 1, NDR  )
        call MOVED (XMUX   , 1, LGX  , XMU    , 1, LG   )
        call MOVED (FRRX   , 1, MRRX , FRR    , 1, MRR  )
        call MOVED (XCOLX  , 1, NCLX , XCOL   , 1, NCL  )
        call MOVED (HNDVX  , 1, NVHX , HNDV   , 1, NVH  )
        call MOVED (VNHX   , 1, NVHX , VNH    , 1, NVH  )
        call MOVED (HNDFX  , 1, NFHX , HNDF   , 1, NFH  )
        call MOVED (FNHX   , 1, NFHX , FNH    , 1, NFH  )
        call MOVED (CQTX   , 1, NCQX , CQT    , 1, NCQ  )
        call MOVED (CQAX   , 1, NCQX , CQA    , 1, NCQ  )
        call MOVED (BXIX   , 1, KBXX , BXI    , 1, KBX  )
        call MOVED (XINKX  , 1, INKX , XINK   , 1, INK  )
        call MOVED (FINKX  , 1, INKX , FINK   , 1, INK  )
        call MOVED (WSLNX  , 1, NLNX , WSLN   , 1, NLN  )
        call MOVED (ZGMX   , 1, NGMX , ZGM    , 1, NGM  )
        call MOVED (DGMZX  , 1, NGMX , DGMZ   , 1, NGM  )
        AL(1) = ONE
C
      else if(MODE.eq.3) then
        call MOVED (EMUFX  , 1, LFX  , EMUF   ,1 ,LF   )
C
      else
        write (MSSLIN(1),100) MODE
  100   format('MODE =',I12,', which is neither 1, 2, nor 3.')
        call HALT  ('REFAULT', 1)
C
      end if
C     !END
      call BYE ('REFAULT')
C
      return
      end
