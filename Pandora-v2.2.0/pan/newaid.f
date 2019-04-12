      subroutine NEWAID
     $(PRNT,LABEL,N,IMG,SO,SN,SNE,RAT,RATE,S)
C
C     Rudolf Loeser, 1998 Oct 01
C---- Prints, for EDWINA.
C     !DASH
      save
C     !DASH
      real*8 RAT, RATE, S, SN, SNE, SO
      integer I, IMG, LUEO, N, NERM
      logical PRINT, PRNT
      character LABEL*25, TITLE*56
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
      external MESHED, LINER, IMGPRNT, MASHED, HI, BYE
C
C               IMG(N), SO(N), SN(N), SNE(N), RAT(N), RATE(N), S(N)
      dimension IMG(*), SO(*), SN(*), SNE(*), RAT(*), RATE(*), S(*)
C     !EJECT
C
      call HI ('NEWAID')
C     !BEG
      PRINT = PRNT.and.(KERMED(5).le.NERM).and.(NERM.gt.0)
C
      write (TITLE,100) LABEL, KERMED(5), NERM
  100 format(A25,5X,'[mssg #',I7,'/max',I7,']')
C
      call MESHED    ('NEWAID', 3)
      if(PRINT) then
        write (LUEO,101) TITLE
  101   format(' ','Editing by EDDIE - line source function.',31X,A56)
        call LINER   (2, LUEO)
        write (LUEO,102)
  102   format(' ',8X,'S as computed',12X,'S(N)',5X,'S(N) edited',
     $             4X,'RAT = S/S(N)',6X,'RAT edited',9X,'S final')
        call LINER   (1, LUEO)
        write (LUEO,103) (I,SO(I),SN(I),SNE(I),RAT(I),RATE(I),S(I),
     $                    I=1,N)
  103   format(5(' ',I5,1P6E16.8/))
C
        KERMED(5) = KERMED(5)+1
C
      else
        write (LUEO,104) TITLE
  104   format(' ','Editing by EDDIE - occurred for: ',A)
        call IMGPRNT (LUEO, IMG, N, 1)
      end if
C
      call MASHED    ('NEWAID')
C     !END
      call BYE ('NEWAID')
C
      return
      end
