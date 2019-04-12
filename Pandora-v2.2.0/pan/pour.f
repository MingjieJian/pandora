      subroutine POUR
     $(XLM,TAUK,OPAC,BHS,SCON,XJNU,KODE,KTRU,XLTIT)
C
C     Rudolf Loeser, 1984 Nov 20
C---- Saves debug checksums, for Continuum calculations.
C     (This is version 2 of POUR.)
C     !DASH
      save
C     !DASH
      real*8 BHS, OPAC, SCON, TAUK, XJNU, XLM, XLTIT
      integer IOMX, IOVER, ITER, ITHSL, KAK1, KAK2, KAK3, KODE, KOUNT,
     $        KTRU, KTYPE, L1, L2, L3, LITER, MO, MXKNT, N
      logical FDB, LINE, PRD, REG
      character TIT*40
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  8),IOMX )
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
      equivalence (LEST(24),LITER)
      equivalence (LEST(19),ITHSL)
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 1),KAK1 )
      equivalence (KAKODS( 2),KAK2 )
      equivalence (KAKODS( 3),KAK3 )
      equivalence (KAKODS( 4),KTYPE)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external  BET, UHU, CHECKER, HI, BYE
      intrinsic mod
C
C               TAUK(N), OPAC(N), BHS(N), SCON(N), XJNU(N)
      dimension TAUK(*), OPAC(*), BHS(*), SCON(*), XJNU(*)
C
      data KOUNT,MXKNT /0, 100/
C
      call HI ('POUR')
C     !BEG
      if(((IOVER.eq.IOMX).and.(MO.gt.0)).or.(IOVER.eq.0)) then
        KOUNT = KOUNT+1
        if(KOUNT.le.MXKNT) then
C
          if(KTRU.eq.1) then
            write (TIT,100) XLM
  100       format(4X,' at',1PE23.16,' true')
          else
            call BET   (2, XLTIT)
            call UHU   (KTYPE, REG, FDB, PRD, LINE)
            L1 = mod(KTYPE, 100)
            if(KODE.eq.2) then
              L2 = mod(KAK3, 100)
              L3 = mod(KAK1, 100)
            else if(KODE.eq.3) then
              L2 = mod(KAK2, 100)
              L3 = mod(KAK3, 100)
            else
              if(LINE) then
                L2 = mod(KAK2, 100)
                L3 = mod(KAK3, 100)
              else
                L2 = mod(KAK1, 100)
                L3 = mod(KAK2, 100)
              end if
            end if
            write (TIT,101) XLM,L1,L2,L3, IOVER,ITER,LITER,ITHSL
  101       format(4X,' @',1PE18.10,1X,3I2,1X,4I2)
          end if
C     !EJECT
          TIT(1:4) = 'OPAC'
          call CHECKER (OPAC, 1, N, TIT)
          TIT(1:4) = 'TAUK'
          call CHECKER (TAUK, 1, N, TIT)
          TIT(1:4) = ' BHS'
          call CHECKER (BHS,  1, N, TIT)
          TIT(1:4) = ' JNU'
          call CHECKER (XJNU, 1, N, TIT)
          TIT(1:4) = 'SCON'
          call CHECKER (SCON, 1, N, TIT)
        end if
      end if
C     !END
      call BYE ('POUR')
C
      return
      end
