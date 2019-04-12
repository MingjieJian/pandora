      subroutine HAZY
     $(N,LU,J,F,CAPPA,BHSNMS,SIGSTR,ZABS,ZSCA,ZSCR,ZBNM,ZBDN,
     $ OPAC,SIGMA,SCAT,BHSNUM,BHSDEN,BHS)
C
C     Rudolf Loeser, 2005 Mar 25
C---- Updates background data with PRD contributions.
C     (This is version 2 of HAZY.)
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSDEN, BHSNMS, BHSNUM, CAPPA, EBNM, F, OBHS, OBNUM,
     $       OOPAC, OPAC, OSCAT, OSCDN, OSIGM, SCAT, SIGMA, SIGSTR,
     $       ZABS, ZBDN, ZBNM, ZERO, ZSCA, ZSCR
      integer I, IPRDD, J, LU, N
      logical OUTP, PRNT
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
      equivalence (KZQ( 98),IPRDD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  ABJECT, DIVIDE, STARER, LINER, HI, BYE
      intrinsic mod, max
C
C               ZABS(N), ZSCA(N), ZSCR(N), ZBNM(N), CAPPA(N), SIGMA(N),
      dimension ZABS(*), ZSCA(*), ZSCR(*), ZBNM(*), CAPPA(*), SIGMA(*),
C
C               BHSNUM(N), BHSDEN(N), BHSNMS(N), SIGSTR(N), SCAT(N),
     $          BHSNUM(*), BHSDEN(*), BHSNMS(*), SIGSTR(*), SCAT(*),
C
C               BHS(N), ZBDN(N), OPAC(N)
     $          BHS(*), ZBDN(*), OPAC(*)
C
      call HI ('HAZY')
C     !BEG
      OUTP = LU.gt.0
      if(OUTP) then
        call ABJECT (LU)
        call STARER (LU)
        call LINER  (2, LU)
        write (LU,100) J
  100   format(' ',I4,'. frequency'//
     $         ' ',6X,12('-'),' OPAC ',11('-'),' ',11('-'),' SIGMA ',
     $             11('-'),' ',8('-'),' SCAT (or R) ',8('-'),' ',
     $             12('-'),' BHS ',12('-')/
     $         ' ',5X,4(6X,'without',7X,'with PRD',2X))
        call LINER  (1, LU)
      end if
C
      do 102 I = 1,N
        PRNT = (mod(I,IPRDD).eq.1).and.OUTP
C
        OSIGM = F*SIGSTR(I)
        OSCDN = F*CAPPA(I)
        OOPAC = OSIGM+OSCDN
        call DIVIDE (OSIGM, OSCDN, OSCAT)
        OBNUM = F*BHSNMS(I)
        call DIVIDE (OBNUM, OSCDN, OBHS)
C
        OPAC(I)   = OOPAC+ZABS(I)
        SIGMA(I)  = OSIGM+ZSCA(I)
        call DIVIDE (SIGMA(I), (OSCDN+ZSCR(I)), SCAT(I))
        EBNM      = max(ZBNM(I),ZERO)
        BHSNUM(I) = OBNUM+EBNM
        BHSDEN(I) = OSCDN+ZBDN(I)
        call DIVIDE (BHSNUM(I), BHSDEN(I), BHS(I))
C
        if(PRNT) then
          write (LU,101) I,OOPAC,OPAC(I),OSIGM,SIGMA(I),OSCAT,SCAT(I),
     $                     OBHS,BHS(I)
  101     format(' ',I5,1P8E15.7)
        end if
  102 continue
C     !END
      call BYE ('HAZY')
C
      return
      end
