      subroutine ADELA
     $(KILROY,CALLER,ITAU,IU,IL,DUMP)
C
C     Rudolf Loeser, 1994 May 24
C---- Sets up dump for CEIJ calculation.
C     !DASH
      save
C     !DASH
      integer I, IL, ITAU, IU, IUL, JDMCE, JUL, K, LUEO
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
      equivalence (KZQ(147),JDMCE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C     !EJECT
C
      call HI ('ADELA')
C     !BEG
      DUMP = .false.
      if(JDMCE.ne.0) then
        IUL = 100*IU+IL
        K = JDMCE
        JUL = K/1000
        I = K-JUL*1000
C
        if(JUL.eq.0) then
          if(I.eq.ITAU) then
            DUMP = .true.
            if(KILROY) then
              KILROY = .false.
              call MESHED (CALLER, 2)
              write (LUEO,100) ITAU
  100         format(' ','Debug printout for CEIJ, all transitions, ',
     $                   'depth #',I4)
            end if
          end if
C
        else if(I.eq.0) then
          if(JUL.eq.IUL) then
            DUMP = .true.
            if(KILROY) then
              KILROY = .false.
              call MESHED (CALLER, 2)
              write (LUEO,101) IU,IL
  101         format(' ','Debug printout for CEIJ, all depths, ',
     $                   'transition',I4,'/',I2)
            end if
          end if
C
        else
          if((I.eq.ITAU).and.(JUL.eq.IUL)) then
            DUMP = .true.
            if(KILROY) then
              KILROY = .false.
              call MESHED (CALLER, 2)
              write (LUEO,102) ITAU,IU,IL
  102         format(' ','Debug printout for CEIJ, depth #',I4,', ',
     $                   'transition',I4,'/',I2)
            end if
          end if
        end if
      end if
C     !END
      call BYE ('ADELA')
C
      return
      end
