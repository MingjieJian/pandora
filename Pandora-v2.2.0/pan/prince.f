      subroutine PRINCE
     $(NO,LUCS)
C
C     Rudolf Loeser, 1984 Jul 27
C---- Prints debug checksums.
C     !DASH
      save
C     !DASH
      integer I, IQPDC, LUCS, NO
C     !COM
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
      equivalence (IQQ(179),IQPDC)
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C     !EJECT
C---- CHECKS      as of 1989 Jan 25
      integer     NCKSUM,NCKSM
      real*8      CSUM
      character   TSUM*40
      parameter   (NCKSUM=1000)
      dimension   CSUM(NCKSUM), TSUM(NCKSUM)
      common      /CKSUM1/ NCKSM
      common      /CKSUM2/ CSUM
      common      /CKSUM3/ TSUM
C     Strategic array checksums, for debugging.
C     .
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('PRINCE')
C     !BEG
      if(IQPDC.gt.0) then
        call PRIAM (NO, 'CHECKSUMS',9)
        write (NO,100)
  100   format(' ','Debug checksums.')
        call LINER (2, NO)
        write (NO,101) (I,CSUM(I),TSUM(I),I=1,NCKSM)
  101   format(5(' ',I5,5X,1PE24.16,5X,A/))
C
        rewind LUCS
        write (LUCS,102) HEAD,VERSION,NCKSM,NVDSCR
  102   format(' ',A80,F9.3,2I10)
        write (LUCS,103) (VDSCRPT(I),I=1,NVDSCR)
  103   format(' ',A63)
        call LINER (1, LUCS)
        write (LUCS,104) (I,CSUM(I),TSUM(I),I=1,NCKSM)
  104   format(' ',I5,5X,1PE24.16,5X,A)
      end if
C     !END
      call BYE ('PRINCE')
C
      return
      end
