      subroutine ATTIC
C
C     Rudolf Loeser, 1976 Jun 28
C---- Allocates some storage blocks.
C     !DASH
      save
C     !DASH
      integer LEN, LIZ, LUEO
C     !COM
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
C     !DASH
      external  MOODY, LOLIPOP, MESHED, ABORT, HI, BYE
      intrinsic max
C
      call HI ('ATTIC')
C     !BEG
      call MOODY
      LIZ = LI1LEN+LI2LEN+LI3LEN
      call LOLIPOP  (LZOQ, LENPBL)
      LEN = max(LIZ,LENPBL)
C
      if(LEN.gt.ILMIT) then
        call MESHED ('ATTIC', 1)
        write (LUEO,100) LIZ,LI1LEN,LI2LEN,LI3LEN,LENPBL,ILMIT
  100   format(' ','Error in ATTIC:'/
     $         ' ','LInLEN = ',I16,5X,'(',3I10,')'/
     $         ' ','LENPBL = ',I16/
     $         ' ','LNGTHX = ',I16)
        call ABORT
      end if
C     !END
      call BYE ('ATTIC')
C
      return
      end
