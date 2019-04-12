      subroutine GILES
     $(LU)
C     Rudolf Loeser, 1977 Nov 18
C---- Prints additional performance statistics.
C     !DASH
      save
C     !DASH
      real*8 COOTA, XCOOP, XMCOA, ZERO
      integer IVOIT, LU, NVOIT
      character LABEL*36, TITLE*6
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
      equivalence (KZQ( 39),NVOIT)
      equivalence (KZQ( 38),IVOIT)
      equivalence (RZQ(105),XMCOA)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- MATRIX      as of 2006 Sep 06
      integer     PRNSW,EDJSW,KNTIN,KNTED
      real*8      CRITJ,TIMIN,TIMED
      common      /MATRIX1/ PRNSW,EDJSW,KNTIN,KNTED
      common      /MATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
C     CRITJ      "junk" criterion for EDJSW.
C     TIMIN      total time for all matrix inversions.
C     TIMED      total time for all determinants.
C     .
C     !EJECT
C---- VOISTA      as of 1984 Apr 24
      integer     KOUNVC
      common      /VOISTA/ KOUNVC
C     Counts number of times Voigt Function is evaluated.
C     .
C---- ARABIS      as of 1989 Feb 22
      integer     NCOOP,NCOPR
      real*8      COOTM
      common      /ARABIS1/ NCOOP,NCOPR
      common      /ARABIS2/ COOTM
C     NCOOP = number of "bona fide" CO opacity values requested;
C     NCOPR = number of Voigt profile evaluations for CO opacity;
C     COOTM = total time (sec) for "bona fide" values.
C     .
C---- WEITIM      as of 1990 Sep 05
      integer     KALLAM,KALPHI
      real*8      TIMLAM,TIMPHI
      common      /WEITIM1/ KALLAM,KALPHI
      common      /WEITIM2/ TIMLAM,TIMPHI
C     Weight matrix timing data:
C     1) KALLAM   - number of "Lambda-1" operator calculations;
C        TIMLAM   - total time (sec) for (1) calls.
C     2) KALPHI   - number of "Phi" operator calculations;
C        TIMPHI   - total time (sec) for (2) calls.
C     .
C     !DASH
C     !EJECT
      external  LINER, HI, BYE
C
      dimension TITLE(3)
C
      data TITLE /'   low', 'medium', '  high'/
C
      call HI ('GILES')
C     !BEG
      if(LU.gt.0) then
        if(NVOIT.gt.0) then
          call LINER (1, LU)
          if(KOUNVC.le.0) then
            write (LU,100)
  100       format(' ','No Voigt calculations were done.')
          else
            write (LU,101) KOUNVC,TITLE(IVOIT),IVOIT
  101       format(' ','Voigt calculations were done',I12,' times,',
     $               ' by the ',A,'-speed routine.'/
     $             ' ','Three Voigt profile methods are available, ',
     $                 'controlled by IVOIT; in this run IVOIT =',I3/
     $             ' ','The low-speed (Rybicki) routine (IVOIT=1) is ',
     $                 'the most precise, but takes about 10 times ',
     $                 'longer than the high-speed routine.'/
     $             ' ','The medium-speed (Drayson) routine (IVOIT=2) ',
     $                 'takes about 2 times longer than the high-',
     $                 'speed routine; its relative difference'/
     $             ' ',' from the low-speed routine is generally less ',
     $                 'than 0.000001, but sometimes as large ',
     $                 'as 0.0001.'/
     $             ' ','The high-speed (Peytremann) routine (IVOIT=3) ',
     $                 'has a relative difference from the low-speed ',
     $                 'routine that is generally'/
     $             ' ',' less than 0.001, but sometimes as large ',
     $                 'as 0.05.',T114,'(1999 Jul 12)')
          end if
        end if
C     !EJECT
        if((KALLAM+KALPHI).gt.0) then
          call LINER (1, LU)
          if(KALLAM.gt.0) then
            write (LU,102) TIMLAM,KALLAM
  102       format(' ','Lambda-1 operator calculations used ',F9.3,
     $                 ' seconds, and were done',I12,' times.')
          end if
          if(KALPHI.gt.0) then
            write (LU,103) TIMPHI,KALPHI
  103       format(' ','Phi operator calculations      used ',F9.3,
     $                 ' seconds, and were done',I12,' times.')
          end if
        end if
        if((KNTIN+KNTED).gt.0) then
          call LINER (1, LU)
          if(KNTIN.gt.0) then
            write (LU,104) TIMIN,KNTIN
  104       format(' ','Matrix inversions              used ',F9.3,
     $                 ' seconds, and were done',I12,' times.')
          end if
          if(KNTED.gt.0) then
            write (LU,105) TIMED,KNTED
  105       format(' ','Determinant calculations       used ',F9.3,
     $                 ' seconds, and were done',I12,' times.')
          end if
        end if
C
        if(NCOOP.gt.0) then
          XCOOP  =NCOOP
          COOTA  =COOTM/XCOOP
          if(XMCOA.ne.ZERO) then
            LABEL=' using the high-speed Voigt routine.'
          else
            LABEL='.'
          end if
          call LINER (1, LU)
          write (LU,106) NCOOP,NCOPR,LABEL,COOTM,COOTA
  106     format(' ','"Bona fide" CO opacity evaluations were ',
     $               'requested',I12,' times;'/
     $           ' ','they involved',I12,' profile calculations',A/
     $           ' ','These calculations used ',F9.3,' seconds, ',
     $               'or ',F9.3,' seconds/value.')
        end if
      end if
C     !END
      call BYE ('GILES')
C
      return
      end
