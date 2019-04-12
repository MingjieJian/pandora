      subroutine HUGRI
     $(IQCXU,QELSM,MCXK,N,TE,ICXDP,NL,NPQ,LRQ,LCX,XRKH,XRLH,XRK,XRL)
C
C     Rudolf Loeser, 1990 Nov 21
C---- Sets up upper-level charge exchange parameters.
C     !DASH
      save
C     !DASH
      real*8 DWAVE, TE, XRK, XRKH, XRL, XRLH
      integer I, ICXDP, IPEX, IQCXU, K, KODE, LCX, LOOK, LRQ, LUEO,
     $        MCXK, N, NL, NPQ
      logical HYDRO
      character QELSM*8
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !EJECT
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
C     !DASH
C     !EJECT
      external LOOKUC, NEARUD, BONDA, BAKHA, LINER, MESHED, MASHED,
     $         ABORT, HI, BYE
C
C               XRKH(N,NPQLM,NXI), XRK(N,NPQLM), NPQ(NL), LCX(NL),
      dimension XRKH(*),           XRK(*),       NPQ(*),  LCX(NL),
C
C               XRLH(N,NPQLM,NXI), XRL(N,NPQLM), LRQ(NL), TE(N)
     $          XRLH(*),           XRL(*),       LRQ(*),  TE(*)
C
      data DWAVE /6.D3/
C
      call HI ('HUGRI')
C     !BEG
      MCXK = 0
      if(IQCXU.gt.0) then
        HYDRO = QELSM(1:3).eq.'H  '
C----   Set up "charge exchange levels", NPQMX, and KODE
        call BONDA      (HYDRO, NL, NPQ, LRQ, LCX, KODE)
        if(KODE.eq.1) then
C----     Set up "charge exchange code", MCXK
          if(HYDRO) then
            MCXK = -1
          else
            call LOOKUC (NAMXED, NXI, QELSM(1:3), K, LOOK)
            if(LOOK.eq.1) then
              MCXK = K
            end if
          end if
        end if
C
        if(NPQMX.gt.NPQLM) then
          call MESHED   ('HUGRI', 1)
          write (LUEO,100) NAMXED(MCXK),MCXK
  100     format(' ','Error in setting up charge-exchange ',
     $               'calculation for ',A,10X,I10)
          write (LUEO,101) NPQMX,NPQLM
  101     format(' ','The maximum value of n is too large.',
     $               10X,'n-max =',I5,10X,'n-lim =',I5)
          call ABORT
        end if
C
        if(MCXK.ne.0) then
          if((ICXDP.lt.1).or.(ICXDP.gt.N)) then
C----       Get default value of dump depth index
            call NEARUD (TE, N, DWAVE, ICXDP)
          end if
        end if
C     !EJECT
        if(MCXK.eq.-1) then
C----     Massage charge exchange input for Hydrogen
          call BAKHA    (N, XRKH, XRLH, XRK, XRL)
        end if
C
        if(MCXK.gt.0) then
C----     Check whether there are sufficient Hydrogen data
          if(NPQMX.lt.LIMPOP(1)) then
            call MESHED ('HUGRI', 1)
            write (LUEO,100) NAMXED(MCXK),MCXK
            write (LUEO,102) NPQMX,LIMPOP(1)
  102       format(' ','n-max =',I5,10X,'LIMPOP(1) =',I4,
     $                 ': not enough Hydrogen population data ',
     $                 'tables are given.')
            call ABORT
          end if
        end if
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.9)) then
        call MESHED     ('HUGRI', 2)
        call LINER      (1, LUEO)
        write (LUEO,103) IQCXU,QELSM(1:3),HYDRO,NL,KODE,K,LOOK,MCXK,N,
     $                   ICXDP
  103   format(' ','IQCXU =',I1,2X,'QELSM = [',A,']',2X,'HYDRO =',L4,
     $             2X,'NL =',I3/
     $         ' ','KODE =',I2,2X,'K =',I3,2X,'LOOK =',I2,2X,'MCXK =',
     $             I3,2X,'N =',I4,2X,'ICXDP =',I4)
        write (LUEO,104) (I,NPQ(I),LRQ(I),LCX(I),I=1,NL)
  104   format(' ',I3,2X,'N =',I6,2X,'L =',I6,2X,'LCX =',I6)
        write (LUEO,105) NPQMX,LIMPOP(1)
  105   format(' ','NPQMX =',I3,2X,'LIMPOP(1) =',I3)
        call MASHED      ('HUGRI')
      end if
C     !END
      call BYE ('HUGRI')
C
      return
      end
