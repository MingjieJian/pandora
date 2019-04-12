      subroutine MOOSE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1984 Jul 21
C---- Allocates scratch storage for GOONEY.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NG, NL, NVF, NXF
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(47),NVF)
      equivalence (JZQ(48),NXF)
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
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MOOSE')
C     !BEG
      call WGET (IS,  CALLER)
C
      NG = max(NVF,NL)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NL
      IN( 3) = IN( 2)+LENPBL
      IN( 4) = IN( 3)+LENPBL
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+NVF
      IN( 9) = IN( 8)+NVF
      IN(10) = IN( 9)+NVF
C
      IN(11) = IN(10)+NVF
      IN(12) = IN(11)+NXF
      IN(13) = IN(12)+NXF
      IN(14) = IN(13)+NXF
      IN(15) = IN(14)+NG
      MUX    = IN(15)+NL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MOOSE')
C
      return
      end
