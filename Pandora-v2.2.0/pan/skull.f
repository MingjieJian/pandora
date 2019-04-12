      subroutine SKULL
     $(DUMP,KILROY,CALLER,TITLE,IU,IL,I,DMPI)
C
C     Rudolf Loeser, 1987 Oct 23
C---- Prints a dump header: Statistical Equilibrium Calculation.
C     !DASH
      save
C     !DASH
      integer I, IL, IU, LDINT, LUEO
      logical DMPI, DUMP, KILROY
      character CALLER*(*), TITLE*(*)
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
      equivalence (KZQ( 48),LDINT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MINNA, MESHED, LINER, HI, BYE
C
      call HI ('SKULL')
C     !BEG
      call MINNA (DUMP, I, LDINT, DMPI)
      if(DMPI) then
        if(KILROY) then
          KILROY = .false.
          call MESHED (CALLER, 2)
          write (LUEO,100) TITLE,IU,IL
  100     format(' ','Detail printout from ',A,' for transition ',
     $               '(',I2,'/',I2,'), [option SEBUG].')
          if(CALLER.ne.'TEACUP') then
            write (LUEO,101)
  101       format(' ','(Additional printout controlled by option '
     $                 'ARHODMP may also be relevant.)')
          end if
        end if
        call LINER    (2, LUEO)
        write (LUEO,102) I
  102   format(' ',6('*****'),2X,'Depth index I =',I4,2X,6('*****'))
      end if
C     !END
      call BYE ('SKULL')
C
      return
      end
