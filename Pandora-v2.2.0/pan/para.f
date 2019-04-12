      subroutine PARA
     $(SYM,KODE)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Determines whether "SYM" is equal to an entry in
C     the POPSYM table. Returns with KODE .gt. 0 if yes, .eq. 0 if no.
C     When KODE .gt. 0, then its value is the index of the POPSYM entry.
C     !DASH
      save
C     !DASH
      integer KIND, KODE, LOOK
      character SYM*3
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      external LOOKUC, HALT, HI, BYE
C
      call HI ('PARA')
C     !BEG
      call LOOKUC (POPSYM(2), (NPOPS-1), SYM, KIND, LOOK)
      if(LOOK.eq.1) then
        KODE = KIND+1
      else if(LOOK.eq.2) then
        KODE = 0
C
      else
        write (MSSLIN(1),100) LOOK
  100   format('LOOK =',I12,', which is neither 1 nor 2.')
        call HALT ('PARA', 1)
      end if
C     !END
      call BYE ('PARA')
C
      return
      end
