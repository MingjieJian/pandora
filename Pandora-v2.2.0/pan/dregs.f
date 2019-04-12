      subroutine DREGS
     $(EDGT,NUMT,LEVT,LEND,NO,IU,IL,WLO,KLO,WHI,KHI,CORE,DL,K,KODE)
C
C     Rudolf Loeser, 1976 Feb 18
C---- Prints for AMBROSE.
C     !DASH
      save
C     !DASH
      real*8 CORE, DL, EDGT, WHI, WLO
      integer I, IL, IN, IQAIW, IU, K, KHI, KLO, KODE, LEND, LEVT, NO,
     $        NUMT
      logical LONG
C     !COM
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
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
      equivalence (IQQ(286),IQAIW)
C     !DASH
      external  LINER, HI, BYE
      intrinsic max, min
C
C               EDGT(LEND), NUMT(LEND), LEVT(LEND), DL(K)
      dimension EDGT(*),    NUMT(*),    LEVT(*),    DL(*)
C
      call HI ('DREGS')
C     !BEG
      if(NO.gt.0) then
        LONG = IQAIW.le.0
        if(LONG) then
          call LINER   (2, NO)
          write (NO,100) IU,IL,CORE,DL(1),K,DL(K)
  100     format(' ','Continuum Absorption Edges affecting Line ',
     $               'Source Function background calculations for ',
     $               'Transition ',I2,'/',I2/
     $           ' ','     Line core wavelength =',1PE14.6,13X,
     $               'DL( 1) =',E14.6,5X,'DL(',I5,') =',E14.6)
          call LINER   (2, NO)
C     !EJECT
          if(KLO.gt.0) then
            IN = NUMT(KLO)
            write (NO,101) EDGT(KLO),LEVT(KLO),NAMES(IN)
  101       format(' ',27X,1PE14.6,2X,'Level',I3,' edge of ',A10)
            call LINER (1,NO)
          end if
C
          if(KODE.eq.1) then
            write (NO,102) K,WLO
  102       format(' ',7X,'Line Center - DL(',I5,')',1PE14.6)
          else
            write (NO,103) WLO
  103       format(' ',7X,'Line Center + DL( 1)',1PE14.6)
          end if
C
          call LINER   (1, NO)
          do 104 I = (KLO+1),(KHI-1)
            IN = NUMT(I)
            write (NO,101) EDGT(  I),LEVT(  I),NAMES(IN)
  104     continue
C
          call LINER   (1, NO)
          write (NO,105) K, WHI
  105     format(' ',7X,'Line Center + DL(',I5,')',1PE14.6)
C
          if(KHI.le.LEND) then
            call LINER (1, NO)
            IN = NUMT(KHI)
            write (NO,101) EDGT(KHI),LEVT(KHI),NAMES(IN)
          end if
C
C
        else
          write (NO,106) IU,IL
  106     format(' ',5X,'Continuum Absorption Edges occur in ',
     $               'background for Transition ',I2,'/',I2)
        end if
      end if
C     !END
      call BYE ('DREGS')
C
      return
      end
