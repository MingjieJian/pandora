      subroutine FUNNY
     $(XLM,L,KILROY)
C
C     Rudolf Loeser, 2004 Apr 21
C---- Dumps for BUNNY.
C     !DASH
      save
C     !DASH
      real*8 XLM, dummy
      integer L, LUEO, jummy
      logical KILROY
C     !COM
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, LYDIA, HI, BYE
C
      call HI ('FUNNY')
C     !BEG
      if(KILROY) then
        write (LUEO,100) XLM
  100   format(' ','Details of calculation of background Oxygen-I ',
     $             'lines at wavelength =',1PE20.13//
     $         ' ','Built-in O-I lines data:')
        call LINER (1, LUEO)
        call LYDIA (LUEO, MOXL, 1, OXMAS, OXSKE, IUOX, ILOX, OXWLO,
     $              OXWVL, OXWHI, OXNUU, OXPU, OXNUL, OXPL, OXAUL,
     $              OXCRD, OXCVW, OXCSK, jummy, dummy, dummy, 'FUNNY')
        KILROY = .false.
      end if
      call LINER   (2, LUEO)
      write (LUEO,101) L,OXWVL(L)
  101 format(' ','OOOOOOOOOO Line #',I3,' at',1PE20.13,' Angstroms')
C     !END
      call BYE ('FUNNY')
C
      return
      end
