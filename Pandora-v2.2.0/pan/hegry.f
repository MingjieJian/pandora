      subroutine HEGRY
C
C     Rudolf Loeser, 1988 May 04
C---- Checks the "Pop-to-Run" indices.
C     !DASH
      save
C     !DASH
      integer I, JPOP, K, LUEO, MRTPX, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
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
      external LINER, ABORT, MESHED, HI, BYE
C     !EJECT
C
      call HI ('HEGRY')
C     !BEG
      if(JPOP.gt.0) then
C
        do 100 I = 1,NPOPS
          if(IUPOP(I).gt.0) then
            MRTPX = LIMPOP(I)
            goto 103
          end if
  100   continue
        call MESHED ('HEGRY', 1)
        write (LUEO,101) LIMPOP
  101   format(' ','LIMPOP =',20I5)
        call LINER  (LUEO, 1)
        write (LUEO,102) JPOP
  102   format(' ','but JPOP =',I12,'. This does not make sense.')
        call ABORT
C
  103   continue
        MRTPA = 1
        do 104 I = 1,MRTPX
          if(MRTP(I).le.0) goto 105
          MRTPA = I
  104   continue
  105   continue
C
        K = 0
        do 106 I = 1,MRTPA
          if(MRTP(I).gt.NL) then
            K = K+1
          end if
  106   continue
C
        if((MRTPA.lt.2).or.(K.gt.0)) then
          call MESHED ('HEGRY', 1)
          write (LUEO,107)
  107     format(' ','"Run-to-Pop" indices:')
          write (LUEO,108) (MRTP(I),I=1,MRTPM)
  108     format(' ',25I5)
          call LINER (1, LUEO)
          write (LUEO,109) MRTPA,K
  109     format(' ','MRTPA =',I5,10X,'KOUNT =',I5/
     $           ' ','Error: at least the first two indices must ',
     $               'correspond to indices of actual levels of the ',
     $               'ion of the run.')
          call ABORT
        end if
C
      end if
C     !END
      call BYE ('HEGRY')
C
      return
      end
