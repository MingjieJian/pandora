      subroutine ENID
     $(NO,N,IQAN1,KAMB, VM,VBMB,PALBET,PBETAL,PGMBET,PBETGM)
C
C     Rudolf Loeser, 1998 Dec 30
C---- Prints "diffusion" input arrays, for PATCH.
C     (This is version 2 of ENID.)
C     !DASH
      save
C     !DASH
      real*8 PALBET, PBETAL, PBETGM, PGMBET, VBMB, VM
      integer IQAN1, KAMB, N, NO
C     !DASH
      external LINER, PRIVET, HI, BYE
C
C               VBMB(N), PALBET(N), PBETAL(N), PGMBET(N), PBETGM(N),
      dimension VBMB(*), PALBET(*), PBETAL(*), PGMBET(*), PBETGM(*),
C
C               VM(N)
     $          VM(*)
C     !EJECT
C
      call HI ('ENID')
C     !BEG
      write (NO,100)
  100 format(' ','VM = mass motion velocity')
      call PRIVET     (NO,VM,N)
C
      call LINER      (1,NO)
      write (NO,102)
  102 format(' ','VBMB = diffusion velocity of hydrogen relative ',
     $           'to helium')
      call PRIVET     (NO,VBMB,N)
C
      if(IQAN1.gt.0) then
        if(KAMB.eq.3) then
          call LINER  (1,NO)
          write (NO,103)
  103     format(' ','PALBET = term from He-I for He-II')
          call PRIVET (NO,PALBET,N)
C
          call LINER    (1,NO)
          write (NO,104)
  104     format(' ','PBETAL = term from He-I for He-II')
          call PRIVET (NO,PBETAL,N)
C
        else if(KAMB.eq.2) then
          call LINER    (1,NO)
          write (NO,105)
  105     format(' ','PGMBET = term from He-II for He-I')
          call PRIVET (NO,PGMBET,N)
C
          call LINER    (1,NO)
          write (NO,106)
  106     format(' ','PBETGM = term from He-II for He-I')
          call PRIVET (NO,PBETGM,N)
        end if
      end if
C     !END
      call BYE ('ENID')
C
      return
      end
