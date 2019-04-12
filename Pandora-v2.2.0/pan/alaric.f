      subroutine ALARIC
     $(BDIJ,IMG,FO,N,NL,IFUDGE,LEGEND,EDITED)
C
C     Rudolf Loeser, 1981 Feb 13
C---- Edits basic B-ratios.
C     !DASH
      save
C     !DASH
      real*8 BDIJ, FO, ZERO
      integer IFUDGE, IJ, IMG, J, KMSS, KNT, LUEO, N, NERM, NL, jummy
      logical BAD, EDITED, KILROY
      character LAB*60, LEGEND*33, TFUJ*10, qummy*1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 95),NERM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- BURNET      as of 1995 Sep 08
      integer     NURBET,KERMED
      parameter   (NURBET=12)
      dimension   KERMED(NURBET)
      common      /BURNET/ KERMED
C     Counts of error messages from EDITH, for various contexts:
C      1 - "optical depth"          2 - basic b-ratios
C      3 - PRD QSF                  4 - whole-profile S
C      5 - line source function     6 - J-bar
C      7 - "Lyman" EP1              8 - "Lyman" RK
C      9 - b-values                10 - net radiative bracket - "rho"
C     11 - GTN or TAU-integrand    12 - S-from-N
C     .
C     !DASH
C     !EJECT
      external  EDITH, WOBLE, MESHED, PLUSD, PRIVET, LINER, MASHED,
     $          ONE1, HI, BYE
      intrinsic max, min
C
C               BDIJ(N,NL), IMG(N), FO(N)
      dimension BDIJ(N,*),  IMG(*), FO(*)
C
      dimension TFUJ(2)
C
      data KMSS /1/
      data TFUJ /'no fudging', 'may fudge '/
C
      call HI ('ALARIC')
C     !BEG
      IJ = max(min(IFUDGE,1),0)+1
C
      KILROY = .true.
      do 103 J = 2,NL
        write (LAB,100) LEGEND,J,TFUJ(IJ)
  100   format(A33,2X,', ratio',I3,'/1, ',A10)
C
        call PLUSD     (BDIJ(1,J), 1, N, KNT)
        if(KNT.le.0) then
          call MESHED  ('ALARIC', 2)
          write (LUEO,101) LAB
  101     format(' ',A,'  --  is all bad:')
          call PRIVET  (LUEO, BDIJ(1,J), N)
          call LINER   (1, LUEO)
          write (LUEO,102)
  102     format(' ','and will be set all = 1.')
          call MASHED  ('ALARIC')
C
          call ONE1    (BDIJ(1,J), N)
          EDITED = .true.
        else
C
          call EDITH   (BDIJ(1,J), N, ZERO, 2, 2, KMSS, LAB, IMG, FO,
     $                  KERMED(2), NERM, BAD)
          if(BAD.and.(KMSS.le.0)) then
            call WOBLE (IMG, N, LAB, 'ALARIC', KILROY, 2)
          end if
          if(BAD) then
            EDITED = .true.
          end if
C
        end if
  103 continue
      call WOBLE       (jummy, jummy, qummy, 'ALARIC', KILROY, 3)
C     !END
      call BYE ('ALARIC')
C
      return
      end
