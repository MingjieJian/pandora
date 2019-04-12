      subroutine DYNAR
     $(LUMR,Z,PALBET,PBETAL,PBETGM,PGMBET)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts He-I diffusion data into restart file.
C     !DASH
      save
C     !DASH
      real*8 PALBET, PBETAL, PBETGM, PGMBET, Z
      integer KAMB, KION, KVLG, LUMR, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
C     !EJECT
      external  BUNT, YARDEN, HI, BYE
      intrinsic max
C
C               Z(N), PALBET(N), PBETAL(N), PBETGM(N), PGMBET(N)
      dimension Z(*), PALBET(*), PBETAL(*), PBETGM(*), PGMBET(*)
C
      call HI ('DYNAR')
C     !BEG
      KION = max(KAMB,KVLG)
      if((KION.eq.2).or.(KION.eq.3)) then
C
        call YARDEN (LUMR, 1, 'PALBET - PBETGM')
C
        write (LUMR,100) N
  100   format('N (',I4,' ) > ')
        call BUNT   (LUMR, Z, 'Z')
        write (LUMR,101) HEAD
  101   format(A80)
C
        if(KION.eq.2) then
          call BUNT (LUMR, PALBET, 'PALBET')
          call BUNT (LUMR, PBETAL, 'PBETAL')
        else
          call BUNT (LUMR, PGMBET, 'PGMBET')
          call BUNT (LUMR, PBETGM, 'PBETGM')
        end if
C
        call YARDEN (LUMR, 2, 'PALBET - PBETGM')
C
      end if
C     !END
      call BYE ('DYNAR')
C
      return
      end
