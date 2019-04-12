      subroutine ADHEMAR
     $(KILROY,CALLER,ITAU,LEV,DUMP)
C
C     Rudolf Loeser, 1994 May 24
C---- Sets up dump for CII calculation.
C     !DASH
      save
C     !DASH
      integer I, ITAU, IXNCS, J, JDMCI, K, LEV, LUEO, MO
      logical DUMP, KILROY
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
      equivalence (KZQ(146),JDMCI)
      equivalence (KZQ(144),IXNCS)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external MESHED, HI, BYE
C
      call HI ('ADHEMAR')
C     !BEG
      DUMP = .false.
C
      if((JDMCI.ne.0).and.(IXNCS.ne.0).and.(MO.gt.0)) then
        K = JDMCI
        J = K/1000
        I = K-J*1000
C
        if((J.eq.0).and.(I.eq.ITAU)) then
          DUMP = .true.
          if(KILROY) then
            KILROY = .false.
            call MESHED (CALLER, 2)
            write (LUEO,100) ITAU
  100       format(' ','Debug printout for CII, all levels, ',
     $                 'depth #',I4)
          end if
C
        else if((I.eq.0).and.(J.eq.LEV)) then
          DUMP = .true.
          if(KILROY) then
            KILROY = .false.
            call MESHED (CALLER, 2)
            write (LUEO,101) LEV
  101       format(' ','Debug printout for CII, all depths, ',
     $                 'level #',I4)
          end if
C
        else if((I.eq.ITAU).and.(J.eq.LEV)) then
          DUMP = .true.
          if(KILROY) then
            KILROY = .false.
            call MESHED (CALLER, 2)
            write (LUEO,102) ITAU,LEV
  102       format(' ','Debug printout for CII, depth #',I4,', ',
     $                 'level #',I4)
          end if
        end if
C
      end if
C     !END
      call BYE ('ADHEMAR')
C
      return
      end
