      subroutine JOSIAH
     $(IU,IL,NO,CALLER,DUMP)
C
C     Rudolf Loeser, 1990 Oct 02
C---- Sets up debug printout for HOSEAH.
C     (This is version 3 of JOSIAH.)
C     !DASH
      save
C     !DASH
      integer IBRDP, IL, IU, LUEO, MS, NO, NS
      logical DUMP
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
      equivalence (KZQ(115),IBRDP)
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
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
      call HI ('JOSIAH')
C     !BEG
      DUMP = .false.
C
      if(IBRDP.gt.0) then
        if((IU.eq.MS).and.(IL.eq.NS)) then
          DUMP = NO.gt.0
        end if
      end if
C
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100) IU,IL
  100   format(' ','Details of ion collision-broadening calculation ',
     $             'for transition (',I2,'/',I2,')')
      end if
C     !END
      call BYE ('JOSIAH')
C
      return
      end
