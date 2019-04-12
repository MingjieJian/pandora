      subroutine EMEER
     $(XLAM,ISW,KOPAC,NOPAC,AR,N,NO,TITLE)
C
C     Rudolf Loeser, 1981 Jun 16
C---- Prints Absorber/Emitter arrays, for debugging.
C     (This is version 3 of EMEER.)
C     !DASH
      save
C     !DASH
      real*8 AR, XLAM
      integer I, ISW, J, JE, JS, KNT, KOPAC, N, NO, NOPAC
      logical JAR
      character TITLE*(*)
C     !DASH
      external  LINER, NAUGHTD, HI, BYE
      intrinsic min
C
C               AR(Nopac,N), ISW(Nopac), KOPAC(Nopac)
      dimension AR(NOPAC,*), ISW(*),     KOPAC(*)
C
C
      call HI ('EMEER')
C     !BEG
      if(NO.gt.0) then
        call LINER       (2, NO)
        write (NO,100) TITLE,XLAM
  100   format(' ','Details of ',A,' at wavelength',1PE20.12)
C
        JE = 0
  101   continue
          JS  = JE+1
          JE  = min(JE+8,N)
          KNT = (JE-JS)+1
          call LINER     (1, NO)
          write (NO,102) (J,J=JS,JE)
  102     format(' ',4X,8I15)
C
          do 104 I = 1,NOPAC
            call NAUGHTD (AR(I,JS), NOPAC, KNT, JAR)
            if(.not.JAR) then
              write (NO,103) ISW(I),KOPAC(I),I,(AR(I,J),J=JS,JE)
  103         format(' ',2I2,' ',I2,:,1P8E15.7)
            else
              write (NO,103) ISW(I),KOPAC(I),I
            end if
  104     continue
C
        if(JE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('EMEER')
C
      return
      end
