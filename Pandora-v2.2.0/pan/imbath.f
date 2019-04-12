      subroutine IMBATH
     $(IMUX)
C
C     Rudolf Loeser, 1998 Jan 13
C---- Records storage usage of TANGO: special case.
C     (This is version 2 of IMBATH.)
C     !DASH
      save
C     !DASH
      integer IMUX, LIBDT
C     !COM
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
C
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
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
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
C
C---- WORLD       as of 2002 Jun 04
C
      integer     LISTK
      parameter   (LISTK = 100)
      integer     ISTCK,INEXT,ILMIT,IUMAX,IUKNT
      dimension   ISTCK(LISTK)
      common      /WORLD/ ISTCK,INEXT,ILMIT,IUMAX,IUKNT
C     Management of floating point working/scratch storage in X
C     - ISTCK is the allocation stack
C     - INEXT is the stack index for the next allocation
C     - ILMIT is the length of X
C     - IUMAX and IUKNT are cumulative usage statistics.
C     .
C
C---- IWORLD      as of 2002 Jun 04
C
      integer     LJSTK
      parameter   (LJSTK = 100)
      integer     JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
      dimension   JSTCK(LJSTK)
      common      /IWORLD/ JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
C     Management of integer working/scratch storage in IX
C     - JSTCK is the allocation stack
C     - JNEXT is the stack index for the next allocation
C     - JLMIT is the length of IX
C     - JUMAX and JUKNT are cumulative usage statistics.
C     .
C     !DASH
      external  HI, BYE
      intrinsic max
C
      call HI ('IMBATH')
C     !BEG
      LIBDT = LI1LEN+LI2LEN+LI3LEN
      IUMAX = KMXWAV+max(LSHF,LENPBL,LIBDT)
      JUMAX = IMUX
C     !END
      call BYE ('IMBATH')
C
      return
      end
