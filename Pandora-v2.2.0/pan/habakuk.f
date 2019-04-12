      subroutine HABAKUK
     $(KK,XK,GK,XNU,XNUC,KOLEV,GAUNT)
C
C     Rudolf Loeser, 2005 Jul 22
C---- Computates Lyman GK.
C     !DASH
      save
C     !DASH
      real*8 GAUNT, GK, XK, XNU, XNUC
      integer I, IP, KK, KOLEV
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
      external HALT, GUNK, HI, BYE
C
C               XK(KK), GK(KK), XNU(NSL), XNUC(NSL), GAUNT(MAXDATL)
      dimension XK(*),  GK(*),  XNU(*),   XNUC(*),   GAUNT(*)
C
      call HI ('HABAKUK')
C     !BEG
C---- Find index of relevant Population Data Block
      IP = 0
      do 100 I = 1,NPOPS
        if(IUPOP(I).gt.0) then
          IP = I
        end if
  100 continue
C
      if(IP.eq.0) then
        write (MSSLIN(1),101)
  101   format('Lyman calculation may not be done in a run which ',
     $         'does not have "POPUP" on.')
        call HALT ('HADRE', 1)
      end if
C
C---- Compute GK
      call GUNK   (XK, KK, IP, XNU, XNUC, KOLEV, GAUNT, GK)
C     !END
      call BYE ('HABAKUK')
C
      return
      end
