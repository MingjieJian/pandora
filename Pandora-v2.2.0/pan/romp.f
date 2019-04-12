      subroutine ROMP
     $(XPBL)
C
C     Rudolf Loeser, 1989 Oct 27
C---- Initializes the Population Data Blocks.
C     (This is version 2 of ROMP.)
C     !DASH
      save
C     !DASH
      real*8 ONE, XPBL, ZERO
      integer I, LLIUP, LLNPOP, jummy
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
      equivalence (LZOQ( 1),LLNPOP)
      equivalence (LZOQ( 2),LLIUP )
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SET1, POPIO, HI, BYE
C
C               XPBL(Lenpbl)
      dimension XPBL(*)
C
      call HI ('ROMP')
C     !BEG
C---- Initialize common buffer contents.
      call SET1    (XPBL, LENPBL, (-ONE))
C
C---- Write initial Population Data Block for each ion in turn
      do 100 I = 1,NPOPS
        XPBL(LLNPOP) = I
        XPBL(LLIUP)  = IUPOP(I)
        call POPIO ('WRITE', jummy, XPBL)
  100 continue
C
C---- "Invalidate" surviving buffer contents
      XPBL(LLNPOP) = ZERO
C     !END
      call BYE ('ROMP')
C
      return
      end
