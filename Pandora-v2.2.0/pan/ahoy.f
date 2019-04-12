      subroutine AHOY
     $(LUPR,XPBL)
C
C     Rudolf Loeser, 1980 Oct 02
C---- Produces populations-related restart data.
C     (This is version 2 of AHOY.)
C     !DASH
      save
C     !DASH
      real*8 XPBL
      integer I, IQHSE, LIMP, LLBD, LLPOPK, LLPOPN, LUPR, MODE, N
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
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
      equivalence (LZOQ( 5),LLBD  )
C     !EJECT
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
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
      equivalence (IQQ( 16),IQHSE)
C     !DASH
      external POPIO, BUNT, PANT, HI, BYE
C
C               XPBL(Lenpbl)
      dimension XPBL(*)
C
      data MODE /1/
C     !EJECT
C
      call HI ('AHOY')
C     !BEG
      do 101 I = 1,NPOPS
        if(LENPOP(I).gt.0) then
C
          if((IUPOP(I).gt.0).or.(IQHSE.gt.0)) then
  100       format(A80)
C
            LIMP = LIMPOP(I)
            call POPIO  ('READ', I, XPBL)
C
            write (LUPR,100) HEAD
            call BUNT   (LUPR, XPBL(LLPOPK),                KLABPI(I))
            call PANT   (LUPR, XPBL(LLPOPN), N, LIMP, MODE, NLABPI(I))
C
            if(IUPOP(I).gt.0) then
              write (LUPR,100) HEAD
              call PANT (LUPR, XPBL(LLBD),   N, LIMP, MODE, BLABPI(I))
            end if
          end if
C
        end if
  101 continue
C     !END
      call BYE ('AHOY')
C
      return
      end
