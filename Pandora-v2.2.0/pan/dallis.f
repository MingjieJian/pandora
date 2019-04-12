      subroutine DALLIS
     $(SYM,KODE)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Determines whether or not a nonLTE ETA table must be fudged.
C     Returns with KODE=1 if yes, KODE=0 if no.
C     !DASH
      save
C     !DASH
      integer I, IN, JYDRO, KODE, LUEO
      character SYM*3
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IUPOP( 1),JYDRO)
C     !DASH
      external MESHED, ABORT, HI, BYE
C     !EJECT
C
      call HI ('DALLIS')
C     !BEG
      KODE = 0
C
C---- If Hydrogen update run, then no.
      if(JYDRO.le.0) then
C----   Find index of the ion whose populations are being updated.
        IN = 0
        do 100 I = 2,NPOPS
          if(IUPOP(I).gt.0) then
            IN = I
          end if
  100   continue
C
        if(IN.gt.0) then
C----     If the current nonLTE metal .eq. the popup ion, then no.
          if(SYM.ne.POPSYM(IN)) then
            KODE = 1
          end if
C
        else
C----     IN=0 is inconsistent, since PEARL is called only if
C         this is a population update run.
          call MESHED ('PEARL/DALLIS', 1)
          write (LUEO,101) IUPOP,SYM
  101     format(' ','This calculation is done only for population ',
     $               'update runs, but no update signal is now set?'//
     $           ' ','IUPOP =',20I5//
     $           ' ','SYM = ',A3//
     $           ' ','So how did control get here? (The Black Hole of ',
     $               'Calcutta.)')
          call ABORT
        end if
C
      end if
C     !END
      call BYE ('DALLIS')
C
      return
      end
