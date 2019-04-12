      subroutine MONK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Jul 27
C---- Allocates scratch storage for BUSY.
C     (This is version 3 of MONK.)
C     !DASH
      save
C     !DASH
      integer IBLK, IN, IS, IZAU, KWC, LZM, MRS, MUX, N, NFH, NSL, NT,
     $        NVX, NZM
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(13),LZM)
      equivalence (JZQ(14),NZM)
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(46),KWC)
      equivalence (JZQ(29),MRS)
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(31),NFH)
      equivalence (JZQ(42),NVX)
C
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
C     .
      equivalence
     $(ISHF( 1),IIIMR ),(ISHF( 2),IIILR ),(ISHF( 3),IIILZA),
     $(ISHF( 4),IIIBNL),(ISHF( 5),IIIBNU),(ISHF( 6),IIIBNE),
     $(ISHF( 7),IIINLP)
C     .
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
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
C     !EJECT
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MONK')
C     !BEG
      call WGET (IS,  CALLER)
C
      IBLK = max(LI2LEN,LENPBL)
      IZAU = LZM*NZM
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+LSHF
      IN( 3) = IN( 2)+IZAU
      IN( 4) = IN( 3)+KWC
      IN( 5) = IN( 4)+NT
      IN( 6) = IN( 5)+LI3LEN
      IN( 7) = IN( 6)+IBLK
      IN( 8) = IN( 7)+NFH
      IN( 9) = IN( 8)+LI1LEN
      IN(10) = IN( 9)+N
      MUX    = IN(10)+NVX
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MONK')
C
      return
      end
