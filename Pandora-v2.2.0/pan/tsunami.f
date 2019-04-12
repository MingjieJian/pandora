      subroutine TSUNAMI
     $(REMARK,NRES,ISLV,LPRD,KONLIC,WAVE)
C
C     Rudolf Loeser, 1995 Apr 25
C---- Encodes an explanatory remark, for SHARI.
C     (This is version 2 of TSUNAMI.)
C     !DASH
      save
C     !DASH
      real*8 WAVE
      integer I, IL, ISLV, IU, KONLIC, LPRD, NRES
      logical WAVEOK
      character BLANK*1, REMARK*9
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
C---- LYMALIM     as of 2005 Jul 08
      real*8      XKWAVU,XKWAVL
      common      /LYMALIM/ XKWAVU,XKWAVL
C     Wavelength limits (Angstroms) for "Lyman" calculation.
C     .
C     !DASH
      external HI, BYE
C
      call HI ('TSUNAMI')
C     !BEG
      REMARK = BLANK
      WAVEOK = (WAVE.ge.XKWAVL).and.(WAVE.le.XKWAVU)
C
      if(LPRD.gt.0) then
C
        IU = KONLIC/100
        IL = KONLIC-100*IU
        write (REMARK,100) IU,IL
  100   format('PRD',I3,'/',I2)
C
      else if((NRES.gt.0).and.WAVEOK) then
C
        do 102 I = 1,NPOPS
          if(NRES.eq.KAPNO(I)) then
            write (REMARK,101) POPSYM(I),ISLV
  101       format('Ly(',A3,')',I2)
            goto 103
          end if
  102   continue
        REMARK = 'Ly(Huh?) '
C
  103   continue
      end if
C     !END
      call BYE ('TSUNAMI')
C
      return
      end
