      subroutine HEARTH
     $(N,NW,NEW,LTYPE,WAVE,WVNM,WTAB,SCON,OPAC,EWAVE,EWVNM,EWTAB,
     $ ESCON,EOPAC,OK)
C
C     Rudolf Loeser, 1997 Dec 18
C---- Pulls out data for "Continuum Exclipse" calculations.
C     (This is version 2 of HEARTH.)
C     !DASH
      save
C     !DASH
      real*8 EOPAC, ESCON, EWAVE, EWTAB, EWVNM, OPAC, SCON, WAVE, WTAB,
     $       WVNM
      integer I, LTYPE, LUEO, N, NEW, NOW, NW
      logical OK, USE
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external FENNEC, MOVE1, MESHED, MASHED, HI, BYE
C
C               WTAB(Nmkuse), WAVE(Nmkuse), SCON(N,Nmkuse), EWAVE(NEW),
      dimension WTAB(*),      WAVE(*),      SCON(N,*),      EWAVE(*),
C
C               OPAC(N,Nmkuse), ESCON(N,NEW), EOPAC(N,NEW), EWVNM(NEW),
     $          OPAC(N,*),      ESCON(N,*),   EOPAC(N,*),   EWVNM(*),
C
C               WVNM(Nmkuse), LTYPE(Nmkuse), EWTAB(NEW)
     $          WVNM(*),      LTYPE(*),      EWTAB(*)
C
      call HI ('HEARTH')
C     !BEG
      NOW = 0
C
      do 100 I = 1,NW
        call FENNEC (LTYPE(I),USE)
C
        if(USE) then
          NOW = NOW+1
          EWAVE(NOW) = WAVE(I)
          EWVNM(NOW) = WVNM(I)
          EWTAB(NOW) = WTAB(I)
          call MOVE1 (SCON(1,I), N, ESCON(1,NOW))
          call MOVE1 (OPAC(1,I), N, EOPAC(1,NOW))
        end if
C
  100 continue
C
      OK = NOW.eq.NEW
C
      if(.not.OK) then
        call MESHED  ('HEARTH', 3)
        write (LUEO,101) NOW,NEW
  101   format(' ','NOW =',I10,'; NEW =',I10,'; counters mixed up.'/
     $         ' ','Omit the "Continuum Eclipse" calculations.')
        call MASHED  ('HEARTH')
      end if
C     !END
      call BYE ('HEARTH')
C
      return
      end
