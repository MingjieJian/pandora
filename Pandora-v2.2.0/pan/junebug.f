      subroutine JUNEBUG
     $(XLTIT,XNU,CORE)
C
C     Rudolf Loeser, 2004 Jun 10
C---- Checks whether this is a wavelength belonging to a line profile
C     of the ion-of-the-run.
C     If yes, returns CORE = that line-core wavelength;
C     if not, returns CORE = -1.
C
C     (This is version 2 of JUNEBUG.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DNU, ONE, XLTIT, XNU
      integer KAK2, KAK3, KTYPE, NL
      logical FDB, LINE, LOK, PRD, REG, UOK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 2),KAK2 )
      equivalence (KAKODS( 3),KAK3 )
      equivalence (KAKODS( 4),KTYPE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external BET, UHU, ANGIE, HI, BYE
C
C               XNU(NSL)
      dimension XNU(*)
C
      call HI ('JUNEBUG')
C     !BEG
      CORE = -ONE
C
      call BET       (2, XLTIT)
      call UHU       (KTYPE, REG, FDB, PRD, LINE)
      if(LINE) then
        UOK = (KAK2.gt.0).and.(KAK2.le.NL)
        LOK = (KAK3.gt.0).and.(KAK3.le.NL)
        if(UOK.and.LOK) then
          DNU = XNU(KAK2)-XNU(KAK3)
          call ANGIE (DNU, CORE)
        end if
      end if
C     !END
      call BYE ('JUNEBUG')
C
      return
      end
