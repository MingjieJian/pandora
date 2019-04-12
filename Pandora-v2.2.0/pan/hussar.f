      subroutine HUSSAR
     $(XLM,L,KILROY)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Dumps for NELLY.
C     !DASH
      save
C     !DASH
      real*8 XLM, dummy
      integer L, LUEO
      logical KILROY
C     !COM
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, LYDIA, HI, BYE
C
      call HI ('HUSSAR')
C     !BEG
      if(KILROY) then
        write (LUEO,100) XLM
  100   format(' ','Details of calculation of background Helium-I ',
     $             'lines at wavelength =',1PE20.13//
     $         ' ','Built-in He-I lines data:')
        call LINER (1, LUEO)
        call LYDIA (LUEO, MHEE, 1, HEEMAS, HEESKE, IUHEE, ILHEE,
     $              HEEWLO, HEEWVL, HEEWHI, HEENUU, HEEPU, HEENUL,
     $              HEEPL, HEEAUL, HEECRD, HEECVW, HEECSK, 1, dummy,
     $              dummy, 'HUSSAR')
        KILROY = .false.
      end if
      call LINER   (2, LUEO)
      write (LUEO,101) L,HEEWVL(L)
  101 format(' ','EEEEEEEEEE Line #',I3,' at',1PE20.13,' Angstroms')
C     !END
      call BYE ('HUSSAR')
C
      return
      end
