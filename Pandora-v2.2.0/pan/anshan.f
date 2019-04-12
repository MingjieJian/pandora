      subroutine ANSHAN
     $(DUMP,KODNT,IW,WAVCO,INWVC,KAPSMP,NP,NT,NV)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Dumps, for AMBRO.
C     !DASH
      save
C     !DASH
      real*8 WAVCO
      integer I, INWVC, IW, J, JE, JS, K, KAPSMP, KODNT, LUEO, NP, NT,
     $        NV
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  LINER, HI, BYE
      intrinsic mod, min
C
C               KAPSMP(NP,NT,NV)
      dimension KAPSMP(NP,NT,*)
C
      call HI ('ANSHAN')
C     !BEG
      if(DUMP) then
        if(mod(IW,KODNT).eq.0) then
C
          call LINER     (3, LUEO)
          write (LUEO,100) IW,WAVCO,INWVC
  100     format(' ','**********',' Dump of record #',I6,10X,
     $               'Wavelength',1PE14.6,' (',I6,')')
C
          do 106 K = 1,NV
            call LINER   (3, LUEO)
            write (LUEO,101) K
  101       format(' ','***',' K =',I3)
C
            JE = 0
  102       continue
              JS = JE+1
              JE = min(JE+20,NT)
              call LINER (2, LUEO)
              write (LUEO,103) (J,J=JS,JE)
  103         format(' ',3X,20I6)
              call LINER (1, LUEO)
C
              do 105 I = 1,NP
                write (LUEO,104) I,(KAPSMP(I,J,K),J=JS,JE)
  104           format(' ',I3,20I6)
  105         continue
C
            if(JE.lt.NT) goto 102
  106     continue
C
        end if
      end if
C     !END
      call BYE ('ANSHAN')
C
      return
      end
