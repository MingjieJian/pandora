      subroutine MURAY
     $(LU,IQLYM,KOLEV,ICHSW,JDMCI,JDMCE)
C
C     Rudolf Loeser, 1990 Oct 19
C---- Prints a legend for PEACH.
C     !DASH
      save
C     !DASH
      integer ICHSW, IQLYM, JDMCE, JDMCI, JDRP1, KOLEV, LU, jummy
      character QELSM*8, qummy*8
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
      equivalence (QZQ(  2),QELSM)
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(24),JDRP1)
C     !DASH
C     !EJECT
      external LINER, PINA, HI, BYE
C
      call HI ('MURAY')
C     !BEG
      if(LU.gt.0) then
        call LINER   (2, LU)
        write (LU,100) ICHSW,JDMCI,JDMCE
  100   format(' ','[Collisions-with-H switch ICHSW =',I2,
     $             '; CI-dump switch JDMCI =',I9,
     $             '; CE-dump switch JDMCE =',I9,']')
C
        if(QELSM(1:3).ne.'H  ') then
          call LINER (1, LU)
          write (LU,101)
  101     format(' ',7X,'Some default values of RRCP(1) can be ',
     $               'computed using: Verner et al. (1996), ',
     $               'ApJ 465, 487.')
          call PINA  (LU, qummy, jummy)
          if(JDRP1.gt.0) then
            write (LU,102)
  102       format(' ',7X,'This run uses such default values.')
          end if
        end if
        if(IQLYM.gt.0) then
          call LINER (1, LU)
          write (LU,103) KOLEV
  103     format(' ',7X,'The data for level ',I2,' are not used ',
     $               'because option LYMAN is on.')
        end if
      end if
C     !END
      call BYE ('MURAY')
C
      return
      end
