      subroutine ZELTENE
     $(DUMP,I,DMPI)
C
C     Rudolf Loeser, 1990 Oct 04
C---- Sets up debug printout for HOSEAH.
C     !DASH
      save
C     !DASH
      integer I, LDINT, LUEO
      logical DMPI, DUMP
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
      external PINNA, LINER, HI, BYE
C
      call HI ('ZELTENE')
C     !BEG
      DMPI = .false.
      if(DUMP) then
        call PINNA   (I, LDINT, DMPI)
C
        if(DMPI) then
          call LINER (1, LUEO)
          write (LUEO,100) I
  100     format(' ','For depth #',I4)
        end if
      end if
C     !END
      call BYE ('ZELTENE')
C
      return
      end
