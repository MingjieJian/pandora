      subroutine DACIA
     $(QNAME)
C
C     Rudolf Loeser, 1971 Mar 02
C---- Reads population update switches.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer JPOP, KERR, KIND, LOOK, LUEO, MODE, NOION, jummy
      character QNAME*8, QSW*8
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
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(22),JPOP )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MACE, MICE, LOOKUC, MESHED, ABORT, CHLOE, UNMIX, CARMEN,
     $         KIWI, HALT, HI, BYE
C
      call HI ('DACIA')
C     !BEG
      KERR = 0
      call MACE
      call KIWI   (MODE, dummy, jummy, QSW, jummy)
      if(MODE.ne.2) then
        goto 205
      end if
      call UNMIX  (QSW)
      call LOOKUC (TNAMES, NPOPS, QSW, KIND, LOOK)
      if(LOOK.ne.1) then
        goto 202
      end if
      call MICE
      if(NOION.gt.0) then
C----   Error
        write (MSSLIN(1),100) QNAME
  100   format('NAME = ',A8,' does not make sense in a NOION run.')
        call HALT ('DACIA', 1)
      end if
C---- Set switches - specific and general
      IUPOP(KIND) = 1
      JPOP = KIND
      goto 199
C
C---- Process error
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('DACIA', 1)
      write (LUEO,200) TNAMES
  200 format(' ','Trouble reading population update switches.'//
     $       ' ','List of valid switch names:'//
     $      (' ',5X,10A10))
      call CHLOE  (LUEO, QSW, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('DACIA')
C
      return
      end
