      subroutine RUBBLE
     $(X,W,IW)
C
C     Rudolf Loeser, 1970 Feb 11
C---- Sets up default "population data" tables.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer I1, I2, I3, IN, IOTH, IPBL1, IPBL2, IPBL3, IQHNP, IS,
     $        ISTA, IW, JJH1, JJH2N, JOTH, KODE, MOX, NO, NOUT, jummy
      logical lummy
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(124),JJH1 )
C
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
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
      equivalence (IQQ( 36),IQHNP)
C     !DASH
C     !EJECT
      external THAMOS, AMBER, CUCUMBR, BEARD, MYRMEX, RUBICON, HELICON,
     $         OXMORON, POPIO, NIGHT, ATOLL, ZEUS, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IPBL1 ),(IN( 2),IPBL2 ),(IN( 3),IPBL3 )
C
      dimension IOTH(NPI), JOTH(NPI), ISTA(NPI)
C
      data JOTH /0,0,0,0,0, 0,0,0,0,0, 14,0,0,0/
      data IOTH /0,0,0,5,0, 0,0,0,0,0, 13,0,0,0/
      data ISTA /0,1,1,2,0, 1,1,1,1,1,  3,1,0,0/
C
      call HI ('RUBBLE')
C     !BEG
C     (Get, and allocate, W allotment)
      call NIGHT       (IN, IS, MOX, 'RUBBLE')
C     (Initialize populations buffers)
      call POPIO       ('INIT', jummy, W(IPBL1))
      call POPIO       ('INIT', jummy, W(IPBL2))
      call POPIO       ('INIT', jummy, W(IPBL3))
C
C---- Partition functions
      call ATOLL       (X, W, IW)
C
C---- Hydrogen
      call POPIO       ('READ', 1, W(IPBL1))
      call MYRMEX      (1, KODE)
      call BEARD       (X, W, W(IPBL1), KODE)
      call POPIO       ('WRITE', jummy, W(IPBL1))
      call THAMOS      (W(IPBL1), X(JJH1))
C---- Initialize Molecular Hydrogen number density, and
C     adjust H RABDs
      call ZEUS        (NO, IQHNP, NOUT)
      call AMBER       (X, W, W(IPBL1), X(JJH2N), lummy, NOUT)
C     !EJECT
C---- Other Population Ions
      do 100 I1 = 2,NPOPS
        call MYRMEX    (I1, KODE)
        if(ISTA(I1).eq.1) then
C----     One stage of ionization only
          call POPIO   ('READ', I1, W(IPBL1))
          call RUBICON (X, W, W(IPBL1), I1, KODE)
          call POPIO   ('WRITE', jummy, W(IPBL1))
C
        else if(ISTA(I1).eq.2) then
C----     Two stages of ionization simultaneously
          I2 = IOTH(I1)
          call POPIO   ('READ', I1, W(IPBL1))
          call POPIO   ('READ', I2, W(IPBL2))
          call HELICON (X, W, W(IPBL1), I1, W(IPBL2), I2, KODE)
          call POPIO   ('WRITE', jummy, W(IPBL1))
          call POPIO   ('WRITE', jummy, W(IPBL2))
C
        else if(ISTA(I1).eq.3) then
C----     Three stages of ionization simultaneously
          I2 = IOTH(I1)
          I3 = JOTH(I1)
          call POPIO   ('READ', I1, W(IPBL1))
          call POPIO   ('READ', I2, W(IPBL2))
          call POPIO   ('READ', I3, W(IPBL3))
          call OXMORON (X, W, W(IPBL1), I1, W(IPBL2), I2, W(IPBL3), I3,
     $                  KODE)
          call POPIO   ('WRITE', jummy, W(IPBL1))
          call POPIO   ('WRITE', jummy, W(IPBL2))
          call POPIO   ('WRITE', jummy, W(IPBL3))
        end if
C
C----   Adjust C or O RABDs for molecules
        call CUCUMBR   (I1, X, W)
  100 continue
C
C     (Give back W allotment)
      call WGIVE       (X, 'RUBBLE')
C     !END
      call BYE ('RUBBLE')
C
      return
      end
