      subroutine AIMERIC
     $(KILROY,CALLER,LABEL,N)
C
C     Rudolf Loeser, 1994 Jun 10
C---- Writes debug messages when IPEX=22.
C     !DASH
      save
C     !DASH
      integer IPEX, LUEO, MSFGR, MSFQM, MSFQR, MSFRT, N
      logical KILROY
      character CALLER*(*), LABEL*(*)
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
      equivalence (KZQ( 18),IPEX )
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
      equivalence (LEST(16),MSFQR)
      equivalence (LEST(18),MSFQM)
      equivalence (LEST(20),MSFRT)
      equivalence (LEST(21),MSFGR)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, LINER, HI, BYE
C
      call HI ('AIMERIC')
C     !BEG
      if((IPEX.eq.-1).or.(IPEX.eq.22)) then
        if(KILROY) then
          call MESHED (CALLER, 2)
          KILROY = .false.
          write (LUEO,100)
  100     format(' ','Details of S.F. calculation methods summary')
        end if
C
        call LINER    (1, LUEO)
        write (LUEO,101) LABEL,N,MSFQR,MSFQM,MSFRT,MSFGR
  101   format(' ',A,T15,'N=',I6,5X,'MSFQR=',I6,5X,'MSFQM=',I6,5X,
     $             'MSFRT=',I5,5X,'MSFGR=',I6)
      end if
C     !END
      call BYE ('AIMERIC')
C
      return
      end
