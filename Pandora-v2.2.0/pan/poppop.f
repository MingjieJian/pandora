      subroutine POPPOP
C
C     Rudolf Loeser, 1984 Mar 06
C---- Prints the random access file record index
C     for Population Data Blocks.
C     !DASH
      save
C     !DASH
      integer I, IQIXD, LUEO
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
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
      equivalence (IQQ(174),IQIXD)
C     !DASH
      external MESHED, MASHED, LINER, HI, BYE
C
      call HI ('POPPOP')
C     !BEG
      if(IQIXD.gt.0) then
        call MESHED ('POPPOP', 2)
C
        write (LUEO,100)
  100   format(' ','Random access file record index of Population ',
     $             'Data Blocks.'//
     $         ' ',10X,'Name',11X,'Address',6X,'LENPOP',6X,'LIMPOP',
     $             7X,'IUPOP',5X,'IPSWICH',5X,'POPSYM',4X,'KAPNO')
        call LINER  (1, LUEO)
C
        write (LUEO,101) (I,NAMES(I),IBLAD(I),LENPOP(I),LIMPOP(I),
     $                    IUPOP(I),IPSWICH(I),POPSYM(I),KAPNO(I),
     $                    I=1,NPOPS)
  101   format(5(' ',I5,5X,A10,5I12,5X,A3,I12/))
C
        call LINER  (2, LUEO)
        write (LUEO,102)
  102   format(' ',111X,'(Option INDXDMP)')
        call MASHED ('POPPOP')
      end if
C     !END
      call BYE ('POPPOP')
C
      return
      end
