      subroutine JOPLIN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1984 Aug 16
C---- Allocates scratch storage for SAUCE.
C     (This is version 2 of JOPLIN.)
C     !DASH
      save
C     !DASH
      integer IN, INK, IS, IVEC, MHM, MUX, N, NCP, NDT, NVH, NWV
      character CALLER*(*)
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(35),INK)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(54),NVH)
      equivalence (JZQ(17),NWV)
      equivalence (JZQ(21),NDT)
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('JOPLIN')
C     !BEG
      call WGET (IS,  CALLER)
C
      IVEC = max(N,NWV,INK,MHM,NDT)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NVH
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+IVEC
      MUX    = IN( 5)+LENPBL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JOPLIN')
C
      return
      end
