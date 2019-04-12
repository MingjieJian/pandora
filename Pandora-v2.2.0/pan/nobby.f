      subroutine NOBBY
     $(ITAU,DMPD,DMPE,HEAD,JLEV,CALLER)
C
C     Rudolf Loeser, 1991 Mar 22
C---- Sets up dumps, for POLLY.
C     !DASH
      save
C     !DASH
      integer ITAU, ITRFI, JLEV, LUEO
      logical DMPD, DMPE, HEAD
      character CALLER*(*)
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
      equivalence (KZQ( 76),ITRFI)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('NOBBY')
C     !BEG
      DMPE = .false.
      DMPD = .false.
      if(ITRFI.eq.-1) then
        DMPE = .true.
        DMPD = .true.
      else if(ITRFI.eq.ITAU) then
        DMPD = .true.
      end if
      if((DMPD.or.DMPE).and.(.not.HEAD)) then
        call MESHED (CALLER, 2)
        write (LUEO,100) JLEV
  100   format(' ','Calculation of TR-effective for level',I5)
        HEAD = .true.
      end if
C     !END
      call BYE ('NOBBY')
C
      return
      end
