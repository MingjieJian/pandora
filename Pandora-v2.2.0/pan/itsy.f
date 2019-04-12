      subroutine ITSY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 18
C---- Allocates scratch storage for BRAMBLE.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NNL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('ITSY')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNL
      IN( 3) = IN( 2)+NNL
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NNL
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+NNL
      IN( 9) = IN( 8)+LENPBL
      IN(10) = IN( 9)+NNL
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+NNL
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+NNL
      IN(17) = IN(16)+NNL
      IN(18) = IN(17)+NNL
      IN(19) = IN(18)+NNL
      IN(20) = IN(19)+NNL
      IN(21) = IN(20)+NNL
C
      IN(22) = IN(21)+NNL
      IN(23) = IN(22)+NNL
      IN(24) = IN(23)+NNL
      IN(25) = IN(24)+NNL
      IN(26) = IN(25)+NNL
      MUX    = IN(26)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('ITSY')
C
      return
      end
