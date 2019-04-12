      subroutine INDIA
     $(HND,HNDO,R,POPN,POPK,XPBL)
C
C     Rudolf Loeser, 1973 Jun 26
C---- Adjusts Population Data in an HSE run, to remain consistent with
C     newly recomputed HND.
C     Completely new, revised version.
C     !DASH
      save
C     !DASH
      real*8 HND, HNDO, POPK, POPN, R, XPBL, dummy
      integer IPOP, J, N, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
C     !DASH
C     !EJECT
      external ARRDIV, ARRMUL, POPIO, PUPPET, HI, BYE
C
C               HND(N), HNDO(N), POPN(N,LIMP), POPK(N), XPBL(Lenpbl),
      dimension HND(*), HNDO(*), POPN(N,*),    POPK(*), XPBL(*),
C
C               R(N)
     $          R(*)
C
      call HI ('INDIA')
C     !BEG
      call ARRDIV     (HND, HNDO, R, N)
C
      do 101 IPOP = 2,NPOPS
        call POPIO    ('READ', IPOP, XPBL)
C
        do 100 J = 1,LIMPOP(IPOP)
          call ARRMUL (POPN(1,J), R, POPN(1,J), N)
  100   continue
        call ARRMUL   (POPK,      R, POPK,      N)
        call PUPPET   (ICKSM(IPOP), N, LIMPOP(IPOP), POPK, POPN, 0,
     $                 dummy)
C
        call POPIO    ('WRITE', jummy, XPBL)
  101 continue
C     !END
      call BYE ('INDIA')
C
      return
      end
